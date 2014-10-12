open Types

(* print functions *)
val string_of_type : var_type -> string
val string_of_constant : constant -> string
val string_of_expression : expression -> string
val string_of_statement : statement -> string
val string_of_program : program -> string

val print_type : var_type -> unit
val print_constant : constant -> unit
val print_expression : expression -> unit
val print_statement : statement -> unit
val print_program : program -> unit

(* conversion from string to Ast *)
val program_of_lexbuf : Lexing.lexbuf -> program
val program_of_string : string -> program

(* check functions *)
type environment_type =
    |BasicType of var_type
    |FunctionType of (var_type option) * (var_type list)

type environment = (string * environment_type) list
exception Bad_type of string

(* throws a Not_found exception when the variable is not in the environment *)
val type_of_var : environment -> string -> environment_type

val type_of_constant : constant -> var_type

(* throws a Bad_type exception when the expression is uncorrectly typed *)
val type_of_expression : environment -> expression -> var_type

(* throws a Bad_type exception when the program is uncorrectly typed *)
val check_types : program -> unit

(* inline a program *)
val inline : program -> program
