open Pervasives

type bexpr =
    EFalse
  | ETrue
  | EVar of string
  | EAnd of bexpr * bexpr
  | EOr of bexpr * bexpr
  | ENot of bexpr
  | EForall of string * bexpr
  | EExists of string * bexpr

type asst = (string * bool) list

(*Helper used later in bvec*)
let bexpr_of_bool b = if b = true then ETrue else EFalse

(* helper to be used in bvec *)
let xor e1 e2 =  EOr((EAnd((e1), (ENot e2))),  (EAnd((ENot e1),  e2)))

let rec eval a e = match e with
    EFalse  -> false
  | ETrue   -> true
  | EVar str  -> List.assoc str a
  | EAnd(bexpr1, bexpr2) -> eval a bexpr1 && eval a bexpr2
  | EOr (bexpr1, bexpr2) -> eval a bexpr1 || eval a bexpr2
  | ENot(bexpr1)         -> not(eval a bexpr1)
  | EForall(str, bexpr1) -> (eval ((str, true)::a) bexpr1) && (eval ((str, false)::a) bexpr1)
  | EExists(str, bexpr1) -> (eval ((str, true)::a) bexpr1) || (eval ((str, false)::a) bexpr1)


let rec free_vars_not_uniq e = match e with
   EFalse -> []
 | ETrue ->  []
 | EForall (str, bexpr1) -> (List.filter (fun x -> not(x=str)) (free_vars_not_uniq bexpr1))
 | EExists (str, bexpr1) -> (List.filter (fun x -> not(x=str)) (free_vars_not_uniq bexpr1))
 | EVar str -> [str]
 | EOr (e1, e2)  -> List.append (free_vars_not_uniq e1) (free_vars_not_uniq e2)
 | EAnd (e1, e2) -> List.append (free_vars_not_uniq e1) (free_vars_not_uniq e2)
 | ENot (e1) -> free_vars_not_uniq e1

(* TODO:   List.sort_uniq not_uniq_list *)

let free_vars e = List.sort_uniq Pervasives.compare (free_vars_not_uniq e)


let rec sat_helper a fvs e = match fvs with
  | ([]) -> if (eval a e) = true then Some a else None
  | fv::fvtail -> let true_branch  = sat_helper ((fv, true)::a)  fvtail e
                  and false_branch = sat_helper ((fv, false)::a) fvtail e
                  in
                    if true_branch != None
                       then true_branch
                    else
                    if false_branch != None
                       then false_branch
                    else None

let sat e = sat_helper [] (free_vars e) e




(* Debugging code for sat

let a = (sat(EAnd((EVar "x"), (ENot(EVar "x")))));;


let b = sat(EOr(EVar "x", EVar "y"));;

let unsome x = match x with
    Some x -> x
  |  _ -> [];;

let a_straight = unsome b;;

let keys = List.map (fun (k, v) -> string_of_bool(v)) a_straight;;

print_string "sat keys: \n";;

List.iter print_string keys;

print_string "\n\n"


*)
