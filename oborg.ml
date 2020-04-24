(* Playing with oborg. *)

open Core

let kf () =
  let password = Cstruct.of_string "kao3ohBae0quaMohzu5eemaeghei3Gox8zu" in
  let key = Keyfile.load_key ~password "/home/davidb/.config/borg/keys/lint_borgs_test" in
  Keyfile.Key.show key

let () =
  let conf = Configfile.load "/lint/borgs/test/config" in
  printf "%s\n" (Configfile.show conf)
