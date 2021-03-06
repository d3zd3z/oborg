(* Borg interaction testing. *)

open OUnit2

let suite = "suite" >::: [
  "dir" >:: Dir.runtest;
  "hashindex" >:: Hashindex.runtest;
  "borgcmd" >:: Borgcmd.runtest;
  "repo" >:: Repo.runtest;
]

let () = 
  run_test_tt_main suite
