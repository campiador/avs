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

val bexpr_of_bool : bool -> bexpr
val xor : bexpr ->  bexpr -> bexpr

val eval : asst -> bexpr -> bool
val free_vars : bexpr -> string list
val sat : bexpr -> asst option
