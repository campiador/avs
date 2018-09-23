type lexpr =
  | Var of string
  | Lam of string * lexpr
  | App of lexpr * lexpr

let rec output_lexpr () = function
  | Var x -> Printf.sprintf "%s" x
  | Lam(x, e) -> Printf.sprintf "%s.%a" x output_lexpr e
  | App(e1, e2) -> Printf.sprintf "(%a) (%a)" output_lexpr e1 output_lexpr e2

let unparse = output_lexpr ()

let free_vars e = []
let subst x e1 e2 = Var "broken"
let beta e = None
let normal_form e = e
let lexpr_of_int n = Var "broken"
let add e1 e2 = Var "broken"
