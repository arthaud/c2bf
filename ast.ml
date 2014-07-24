open Types
open Ast_parser
open Ast_lexer
open Printf
open Str

(* print functions *)
let string_of_type = function
    |Int -> "int"
    |Bool -> "bool"

let string_of_constant = function
    |IntConst x -> string_of_int x
    |BoolConst true -> "true"
    |BoolConst false -> "false"

let rec string_of_expression = function
    |Var name -> name
    |Const cst -> string_of_constant cst
    |Add(left, right) -> "(" ^ (string_of_expression left) ^ ") + (" ^ (string_of_expression right) ^ ")"
    |Sub(left, right) -> "(" ^ (string_of_expression left) ^ ") - (" ^ (string_of_expression right) ^ ")"
    |Mul(left, right) -> "(" ^ (string_of_expression left) ^ ") * (" ^ (string_of_expression right) ^ ")"
    |Div(left, right) -> "(" ^ (string_of_expression left) ^ ") / (" ^ (string_of_expression right) ^ ")"
    |Minus expr -> "-(" ^ (string_of_expression expr) ^ ")"
    |Inf(left, right)   -> "(" ^ (string_of_expression left) ^ ") < ("  ^ (string_of_expression right) ^ ")"
    |InfEq(left, right) -> "(" ^ (string_of_expression left) ^ ") <= ("  ^ (string_of_expression right) ^ ")"
    |Eq(left, right)    -> "(" ^ (string_of_expression left) ^ ") == ("  ^ (string_of_expression right) ^ ")"
    |NotEq(left, right) -> "(" ^ (string_of_expression left) ^ ") != ("  ^ (string_of_expression right) ^ ")"
    |Sup(left, right)   -> "(" ^ (string_of_expression left) ^ ") > ("  ^ (string_of_expression right) ^ ")"
    |SupEq(left, right) -> "(" ^ (string_of_expression left) ^ ") >= ("  ^ (string_of_expression right) ^ ")"
    |And(left, right) -> "(" ^ (string_of_expression left) ^ ") && (" ^ (string_of_expression right) ^ ")"
    |Or(left, right)  -> "(" ^ (string_of_expression left) ^ ") || (" ^ (string_of_expression right) ^ ")"
    |Not c -> "!(" ^ (string_of_expression c) ^ ")"
;;

let rec
string_of_statement = function
    |Define(var_t, name, expr) -> (string_of_type var_t) ^ " " ^ name ^ " = " ^ (string_of_expression expr) ^ ";"
    |Assign(name, expr) -> name ^ " = " ^ (string_of_expression expr) ^ ";"
    |If(cond, statements, []) ->
        "if (" ^ (string_of_expression cond) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n}"
    |If(cond, statements_true, statements_false) ->
        "if (" ^ (string_of_expression cond) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements_true)) ^
        "\n} else {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements_false)) ^
        "\n}"
    |While(cond, statements) ->
        "while (" ^ (string_of_expression cond) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n}"
    |For(init, cond, incr, statements) ->
        "for (" ^ (string_of_statement init) ^ " , " ^ (string_of_expression cond) ^ " , " ^ (string_of_statement incr) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n}"
and
string_of_program = function
    |[] -> ""
    |statement::[] -> string_of_statement statement
    |statement::program -> (string_of_statement statement) ^ "\n" ^ (string_of_program program)
;;

let print_type vt = print_string (string_of_type vt);;
let print_constant c = print_string (string_of_constant c);;
let print_expression e = print_string (string_of_expression e);;
let print_statement s = print_string (string_of_statement s);;
let print_program p = print_string (string_of_program p);;

(* conversion from string to Ast *)
let program_of_lexbuf lexbuf = nt_program tokenize lexbuf;;
let program_of_string src = program_of_lexbuf (Lexing.from_string src);;

(* check functions *)
type environment = (string * var_type) list
exception Bad_type of string

let type_of_var env name = List.assoc name env;;

let type_of_constant = function
    |IntConst _ -> Int
    |BoolConst _ -> Bool
;;

let rec type_of_expression env expr =
    let type_of_unary_expr op expected_type result_type expr =
        let expr_type = type_of_expression env expr in
        if expr_type = expected_type then result_type
        else raise (Bad_type (sprintf "cannot %s type %s." op (string_of_type expr_type)))
    in
    let type_of_binary_expr op expected_type result_type left right =
        let left_type = type_of_expression env left
        and right_type = type_of_expression env right
        in
            if left_type = expected_type && right_type = expected_type then result_type
            else raise (Bad_type (sprintf "cannot %s types %s and %s." op (string_of_type left_type) (string_of_type right_type)))
    in
    match expr with
        |Var name ->
            (
            try
                type_of_var env name
            with Not_found ->
                raise (Bad_type (sprintf "variable %s is not defined" name))
            )
        |Const cst -> type_of_constant cst
        |Add(left, right) -> type_of_binary_expr "add" Int Int left right
        |Sub(left, right) -> type_of_binary_expr "subtract" Int Int left right
        |Mul(left, right) -> type_of_binary_expr "multiply" Int Int left right
        |Div(left, right) -> type_of_binary_expr "divide" Int Int left right
        |Minus expr       -> type_of_unary_expr "use operator - on" Int Int expr
        |Inf(left, right)   -> type_of_binary_expr "use operator < on" Int Bool left right
        |InfEq(left, right) -> type_of_binary_expr "use operator <= on" Int Bool left right
        |Eq(left, right)    -> type_of_binary_expr "use operator == on" Int Bool left right
        |NotEq(left, right) -> type_of_binary_expr "use operator != on" Int Bool left right
        |Sup(left, right)   -> type_of_binary_expr "use operator > on" Int Bool left right
        |SupEq(left, right) -> type_of_binary_expr "use operator >= on" Int Bool left right
        |And(left, right) -> type_of_binary_expr "use operator && on" Bool Bool left right
        |Or(left, right)  -> type_of_binary_expr "use operator || on" Bool Bool left right
        |Not expr -> type_of_unary_expr "use operator not on" Bool Bool expr
;;

let check_types =
    let rec aux env = function
        |[] -> ()
        |Define(var_t, name, expr)::q ->
            if List.mem_assoc name env then
                raise (Bad_type (sprintf "variable %s already defined." name))
            else if not(var_t = type_of_expression env expr) then
                raise (Bad_type (sprintf "expression of type %s cannot be assigned to variable %s of type %s." (string_of_type (type_of_expression env expr)) name (string_of_type var_t)))
            else aux ((name, var_t)::env) q
        |Assign(name, expr)::q ->
            (
            try
                let var_t = type_of_var env name in
                if not(var_t = type_of_expression env expr) then
                    raise (Bad_type (sprintf "expression of type %s cannot be assigned to variable %s of type %s." (string_of_type (type_of_expression env expr)) name (string_of_type var_t)))
                else
                    aux env q
            with Not_found ->
                raise (Bad_type (sprintf "variable %s is not defined." name))
            )
        |If(cond, statements_true, statements_false)::q ->
            if not(type_of_expression env cond = Bool) then
                raise (Bad_type (sprintf "type %s cannot be used as a condition." (string_of_type (type_of_expression env cond))));
            aux env statements_true;
            aux env statements_false;
            aux env q
        |While(cond, statements)::q ->
            if not(type_of_expression env cond = Bool) then
                raise (Bad_type (sprintf "type %s cannot be used as a condition." (string_of_type (type_of_expression env cond))));
            aux env statements;
            aux env q
        |For(init, cond, incr, statements)::q ->
            aux env [init; While(cond, statements @ [incr])];
            aux env q
        in
        aux [];;
