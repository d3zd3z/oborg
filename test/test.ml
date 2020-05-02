(* Borg interaction testing. *)

open Core

let () = 
  printf "dir:%!";
  Dir.runtest();
  printf " pass\n"
