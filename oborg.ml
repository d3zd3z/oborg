(* Playing with oborg. *)

open Core

let kf () =
  let password = Cstruct.of_string "kao3ohBae0quaMohzu5eemaeghei3Gox8zu" in
  let key = Keyfile.load_key ~password "/home/davidb/.config/borg/keys/lint_borgs_test" in
  Keyfile.Key.show key

(*
let () =
  let conf = Configfile.load "/lint/borgs/test/config" in
  printf "%s\n" (Configfile.show conf)
*)

type il = [`None | `Built of int | `Rebuild of (int * int array)] [@@deriving show]

let () =
  match Sys.get_argv () with
    | [| _; path |] ->
      let repo = Repo.openrepo path in
      printf "%s\n" (Repo.show repo);
      printf "last seg: %s\n" (show_il (Repo.last_segment repo))
    | _ -> failwith "Expecting a path argument"
