open Boolean

type bvec = bexpr list (* low order bit at head of list *)

val int_of_bvec : bvec -> int
val bvec_of_int : int -> bvec
val subst : asst -> bvec -> bvec
val zero : bvec -> bexpr
val bitand : bvec -> bvec -> bvec
val eq : bvec -> bvec -> bexpr
val add : bvec -> bvec -> bvec
