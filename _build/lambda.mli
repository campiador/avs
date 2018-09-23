type lexpr =
  | Var of string
  | Lam of string * lexpr
  | App of lexpr * lexpr

val unparse : lexpr -> string

val free_vars : lexpr -> string list
val subst : string -> lexpr -> lexpr -> lexpr
val beta : lexpr -> lexpr option
val normal_form : lexpr -> lexpr
val lexpr_of_int : int -> lexpr
val add : lexpr -> lexpr -> lexpr
