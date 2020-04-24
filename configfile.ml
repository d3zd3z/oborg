(* The config file *)

open Core

(* The config file looks to be based on the ini format, values are
 * unquoted strings, some are numbers, and continuation lines start
 * with a tab.  We will read expected strings, and store them in a
 * struct, with a fairly strict parser that understands the format as
 * written. *)

module Cstruct = struct
  include Cstruct

  let pp = hexdump_pp
end

type t = {
  (* These seem to be mandatory *)
  version : int;
  segments_per_dir : int;
  max_segment_size : int;
  append_only : bool;
  id : Cstruct.t;

  storage_quota : int option;
  additional_free_space : int option;
  key : Keyfile.Key.t option;
}
[@@deriving show]

(* Squash any lines.  This works on the reversed list of lines, and
 * any line that is a continuation will have the whitespace removed
 * and be appended to the previous line. *)
let squash lines =
  let rec loop result = function
    | [] -> result
    | [x] -> x::result
    | (a::b::xs) when String.is_prefix a ~prefix:"\t" ->
        let ab = b ^ String.subo a ~pos:1 in
        loop result (ab::xs)
    | (x::xs) -> loop (x::result) xs
  in
  loop [] (List.rev lines)

let kw_re = Re.compile (Re.Pcre.re {|^([a-z_]+) = (.*)$|})
let fields = Set.of_list (module String)
  ["version"; "segments_per_dir"; "max_segment_size";
  "append_only"; "id"; "storage_quota"; "additional_free_space";
  "key"]

let decode lines =
  let rec loop fields map = function
    | "[repository]"::xs -> loop fields map xs
    | ""::xs -> loop fields map xs
    | line::xs ->
        begin match Re.exec_opt kw_re line with
          | None -> failwith "Invalid config line"
          | Some pats ->
              let key = Re.Group.get pats 1 in
              let data = Re.Group.get pats 2 in
              if not (Set.mem fields key) then
                failwith (sprintf "Unknown field %S in config" key);
              let fields = Set.remove fields key in
              let map = Map.add_exn map ~key ~data in
              loop fields map xs
        end
    | [] -> map
  in
  loop fields (Map.empty (module String)) lines

let get_int map key = Map.find_exn map key |> Int.of_string
let get_int_opt map key = Map.find map key |> Option.map ~f:Int.of_string
let get_bool map key =
  Map.find map key
  |> Option.map ~f:(fun x -> Int.of_string x |> (<>) 0)
  |> Option.value ~default:false
let get_hex map key =
  Map.find_exn map key
  |> Cstruct.of_hex

let load filename =
  let lines = In_channel.read_lines filename in
  let lines = squash lines in
  let map = decode lines in
  let key = Map.find map "key"
    |> Option.map ~f:(Keyfile.from_base64
      ~password:(Cstruct.of_string "kao3ohBae0quaMohzu5eemaeghei3Gox8zu")) in
  {
    version = get_int map "version";
    segments_per_dir = get_int map "segments_per_dir";
    max_segment_size = get_int map "max_segment_size";
    append_only = get_bool map "append_only";
    id = get_hex map "id";
    storage_quota = get_int_opt map "storage_quota";
    additional_free_space = get_int_opt map "additional_free_space";
    key;
  }
