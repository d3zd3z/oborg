(* Operations using the borg command itself. *)

open Core

(* The encryption modes within Borg.  Uncomment these as we add
 * support in our code for them. *)
let modes = [
  "none";
  "authenticated";
  (* "authenticated-blake2"; *)
  "repokey";
  "keyfile";
  (* "repokey-blake2"; *)
  (* "keyfile-blake2"; *)
]

let make_env dir =
    `Replace [("BORG_BASE_DIR", dir ^/ "base");
      ("BORG_REPO", dir ^/ "repo");
      ("BORG_PASSPHRASE", "evil-simple")]

let init dir mode =
  Shell.run "borg" ["init"; "-e"; mode] ~env:(make_env dir)

let create dir name file =
  Shell.run "borg" ["create"; "-s"; "--list"; "::"^name; file]
    ~env:(make_env dir)

let runtest _ =
  List.iter modes ~f:(fun mode ->
    Dir.with_dir ~f:(fun dir ->
      init dir mode;
      create dir "first" "."))
