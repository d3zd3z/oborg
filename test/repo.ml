(* Test reading repos. *)

open Core

let runtest _ =
  let mode = "repokey" in
  Dir.with_dir ~f:(fun dir ->
    Borgcmd.init dir mode;
    Borgcmd.create dir "first" ".";
    let env = Borg.Repo.empty_env () in
    let env = Borg.Repo.set_env env `Base (dir ^/ "base") in
    let env = Borg.Repo.set_env env `Password "evil-simple" in
    let r = Borg.Repo.openrepo env (dir ^/ "repo") in
    match Borg.Repo.last_segment r with
      | `None -> failwith "No index"
      | `Built n -> printf "Index at %d\n" n
      | `Rebuild _ -> failwith "Rebuild needed")
