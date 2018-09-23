open Boolean

type bvec = bexpr list (* low order bit at head of list *)

let rec int_of_bvec v = match v with
    [] -> 0
  | [Boolean.ETrue] -> 1
  | [Boolean.EFalse] -> 0
  |  x::vtail ->   (int_of_bvec [x] + (2 * int_of_bvec vtail) )

let rec bvec_of_int i = match i with
     0 -> []
   | not_zero -> let b = if (not_zero mod 2) = 1 then Boolean.ETrue else Boolean.EFalse in
              b::(bvec_of_int (not_zero / 2))


(*
let a = (bvec_of_int 10);;
List.iter (fun x -> if x = Boolean.ETrue then (print_string "true") else (print_string "false ")) a
 *)

(* let bexpr_of_bool b = if b = true then Boolean.ETrue else Boolean.EFalse *)

let rec replace_vars a e = match e with
    Boolean.EVar(v_str) -> (if (List.mem_assoc v_str a) then (Boolean.bexpr_of_bool (List.assoc v_str a)) else (Boolean.EVar(v_str)))
  | Boolean.EAnd(expr1, expr2) -> Boolean.EAnd(replace_vars a expr1, replace_vars a expr2)
  | Boolean.EOr(expr1, expr2) -> Boolean.EOr(replace_vars a expr1, replace_vars a expr2)
  | _ -> e (* ETrue and EFalse. Assumption: not taking EExists and EForall*)

let rec subst a v = match v with
    [] -> []
  | bexpr::vtail -> (replace_vars a bexpr)::(subst a vtail)


let rec zero v = match v with
    a::[] -> Boolean.ENot(a)
  | a::tail -> Boolean.EAnd(Boolean.ENot(a), zero tail)
  | _ -> failwith "unexpected input for zero"

let rec bitand v1 v2 = match (v1, v2) with
    ([], []) -> []
  | (v1b::v1tail, v2b::v2tail) -> let b = (Boolean.eval []  (Boolean.EAnd(v1b, v2b))) in
                                  (Boolean.bexpr_of_bool b)::(bitand v1tail v2tail)
  | _ -> failwith "unexpected input on list"

let rec eq_bexpr e1 e2 = match (e1, e2) with
    Boolean.EFalse, Boolean.EFalse -> true
  | Boolean.ETrue, Boolean.ETrue -> true
  | ((Boolean.EVar str1), (Boolean.EVar str2)) -> (str1 = str2)
  | (Boolean.EOr(e11, e12), Boolean.EOr(e21, e22)) -> (eq_bexpr e11 e21) && (eq_bexpr e12 e22)
  | (Boolean.EAnd(e11, e12), Boolean.EAnd(e21, e22)) -> (eq_bexpr e11 e21) && (eq_bexpr e12 e22)
  | _ -> false

let eq v1 v2 = Boolean.bexpr_of_bool (List.for_all2 eq_bexpr v1 v2)



(* let add_bexpr e1 e2 c_i = *)

(* (Boolean.xor((Boolean.xor(e1 ,e2)), c_i)) *)

                          (* let c_o = Booean.EOr Boolean.EAnd(e1, e2) Boolean.EAnd c_i Boolean.xor e1 e2 in*)

let add v1 v2 = let i1 = (int_of_bvec v1) in
                let i2 = (int_of_bvec v2) in
                let sum = i1 + i2 in
                bvec_of_int sum
