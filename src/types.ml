type var_type = Int | Bool

type constant =
    |IntConst of int
    |BoolConst of bool
    |CharConst of char

type expression =
    |Var of string
    |Const of constant
    |Add of expression * expression
    |Sub of expression * expression
    |Mul of expression * expression
    |Div of expression * expression
    |Minus of expression
    |Inf of expression * expression
    |InfEq of expression * expression
    |Eq of expression * expression
    |NotEq of expression * expression
    |Sup of expression * expression
    |SupEq of expression * expression
    |And of expression * expression
    |Or of expression * expression
    |Not of expression
    |ReadChar

type statement =
    |Define of var_type * string * expression
    |Assign of string * expression
    |If of expression * statement list * statement list
    |While of expression * statement list
    |For of statement * expression * statement * statement list
    |WriteChar of expression
    |Block of statement list

type program = statement list
