(* Borg interaction testing. *)

open OUnit2

let suite = "suite" >::: [
  "dir" >:: Dir.runtest;
]

let () = 
  run_test_tt_main suite
