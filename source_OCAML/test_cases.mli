open Syntax
open Binding

type test_case = term list * term
val empty_test_case : test_case
val read_test_cases : string->context->(test_case list * ty)
val match_case : ?debug:bool -> term ->context ->test_case -> bool
