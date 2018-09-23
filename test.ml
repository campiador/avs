open OUnit;;

let t () = assert_equal 1 1;;
let u () = assert_equal 1 1;;

let tbool1 () = assert_equal true (Boolean.eval []  Boolean.ETrue);;

let tbool2 () = assert_equal true (Boolean.eval ["x", true] (Boolean.EVar "x"));;

let tbool3 () = assert_raises Not_found (fun _ -> Boolean.eval [] (Boolean.EVar "x"));;

let tbool4 () = assert_equal false (Boolean.eval [] (Boolean.EAnd (Boolean.ETrue, Boolean.EFalse)));;
let tbool5 () = assert_equal true (Boolean.eval [] (Boolean.ENot(Boolean.EFalse)));;
let tbool6 () = assert_equal true (Boolean.eval [] (Boolean.EOr (Boolean.ETrue, Boolean.EFalse)));;

(* When x is false, then the expression in evar is false, so we expect for all to return false *)
let tbool7 () = assert_equal false (Boolean.eval [] (Boolean.EForall("x", Boolean.EVar "x") ));;

let tbool8 () = assert_equal true (Boolean.eval [] (Boolean.EExists("x", Boolean.EVar "x") ));;


(* free_vars on EExists("x", EOr(EVar "x", EVar "y") should return "y" *)

let tbool9 () = assert_equal ["y"] (Boolean.free_vars (Boolean.EExists("x", Boolean.EOr(Boolean.EVar "x", Boolean.EVar "y"))));;


let tbool10 () = assert_equal None (Boolean.sat(Boolean.EAnd((Boolean.EVar "x"), (Boolean.ENot(Boolean.EVar "x")))));;

(* sat(EOr(EVar "x", EVar "y")) *)
let tbool11 () = assert_equal (Some(["y", true; "x",true])) (Boolean.sat(Boolean.EOr((Boolean.EVar "x"), (Boolean.EVar "y"))));;

(* let tbool12 () = assert_equal Boolean.EFalse (Boolean.xor(Boolean.ETrue));;*)

let suite_boolean = "Boolean" >::: [
  "tbool1: eval a single ETrue" >:: tbool1;
  "tbool2: eval a single EVar" >:: tbool2;
  "tbool3: eval a variable not given in asst" >:: tbool3;
  "tbool4: eval And" >:: tbool4;
  "tbool5: eval Not" >:: tbool5;
  "tbool6: eval Or" >:: tbool6;
  "tbool7: eval forall" >:: tbool7;
  "tbool8: eval exists" >:: tbool8;
  "tbool9: free_vars" >:: tbool9;
  "tbool10: sat expected to return None" >:: tbool10;
  "tbool11: sat expected to return [y,true; x, true]" >:: tbool11;
(*  "tbool12: xor function" >:: tbool12; *)
]


let tbvec1 () = assert_equal 10 (Bvec.int_of_bvec [Boolean.EFalse; Boolean.ETrue; Boolean.EFalse; Boolean.ETrue; Boolean.EFalse]);;

let tbvec2 () = assert_equal [Boolean.EFalse; Boolean.ETrue; Boolean.EFalse; Boolean.ETrue] (Bvec.bvec_of_int 10);;

(*subst ["x", true] [EVar "x"; ETrue; EAnd(EVar "x", EVar "y")] should return [ETrue; ETrue; EAnd(ETrue, EVar "y")].*)
let tbvec3 () = assert_equal [Boolean.ETrue; Boolean.ETrue; Boolean.EAnd(Boolean.ETrue, Boolean.EVar "y")] (Bvec.subst ["x", true] [Boolean.EVar "x"; Boolean.ETrue; Boolean.EAnd(Boolean.EVar "x", Boolean.EVar "y")])


(* zero [EVar "x"; EVar "y"] evaluate to EAnd(ENot(EVar "x"), ENot(EVar "y")) *)
let tbvec4 () = assert_equal (Boolean.EAnd(Boolean.ENot(Boolean.EVar "x"), Boolean.ENot(Boolean.EVar "y"))) (Bvec.zero [Boolean.EVar "x"; Boolean.EVar "y"])

let tbvec5 () = assert_equal [Boolean.EFalse; Boolean.ETrue; Boolean.EFalse] (Bvec.bitand [Boolean.ETrue; Boolean.ETrue; Boolean.ETrue] [Boolean.EFalse; Boolean.ETrue; Boolean.EFalse])


let tbvec6 () = assert_equal Boolean.ETrue (Bvec.eq [Boolean.EFalse; Boolean.ETrue; Boolean.EFalse] [Boolean.EFalse; Boolean.ETrue; Boolean.EFalse])

let tbvec7 () = assert_equal Boolean.EFalse (Bvec.eq [Boolean.EFalse; Boolean.ETrue; Boolean.EFalse] [Boolean.EFalse; Boolean.ETrue; Boolean.ETrue])


let tbvec8()  = assert_equal [Boolean.EFalse; Boolean.ETrue] (Bvec.add [Boolean.ETrue] [Boolean.ETrue])

let suite_bvec = "Bvec" >::: [
  "tbvec1: int_of_bvec should be 10" >:: tbvec1;
  "tbvec2: bvec_of_int 10 should generate correct bvec" >:: tbvec2;
  "tbvec3: subst" >:: tbvec3;
  "tbvec4: zero expression generator" >:: tbvec4;
  "tbvec5: bitand two arrays" >:: tbvec5;
  "tbvec6: eq vecs" >:: tbvec6;
  "tbvec7: eq vecs" >:: tbvec7;
  "tbvec8: add vecs" >:: tbvec8

]

let suite_magic = "Magic" >::: [
  "u" >:: u
]

let suite_lambda = "Lambda" >::: [
  "u" >:: u
]
