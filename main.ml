(* The first four lines just make sure ocamlbuild compiles all the files *)
let _ = Boolean.sat in
let _ = Bvec.int_of_bvec in
let _ = Magic.pad in
let _ = Lambda.unparse in
let _ = Printf.printf "Boolean:\n" in
let _ = OUnit.run_test_tt_main Test.suite_boolean in
let _ = Printf.printf "Bvec:\n" in
let _ = OUnit.run_test_tt_main Test.suite_bvec in
let _ = Printf.printf "Magic:\n" in
let _ = OUnit.run_test_tt_main Test.suite_magic in
let _ = Printf.printf "\n\nLambda:\n" in
let _ = OUnit.run_test_tt_main Test.suite_lambda in
  ()
