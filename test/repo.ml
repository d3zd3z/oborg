(* Test reading repos. *)

open Core

let runtest _ =
  let mode = "repokey" in
  Dir.with_dir ~f:(fun dir ->
    Borgcmd.init dir mode;
    Borgcmd.create dir "first" ".";
    Borg.Repo.set_base (dir ^/ "base");
    Borg.Repo.set_password "evil-simple";
    let r = Borg.Repo.openrepo (dir ^/ "repo") in
    match Borg.Repo.last_segment r with
      | `None -> failwith "No index"
      | `Built n -> printf "Index at %d\n" n
      | `Rebuild _ -> failwith "Rebuild needed")
