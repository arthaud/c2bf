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
    |CharConst c -> sprintf "'%c'" c

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
    |ReadChar -> "read_char()"
    |Call(name, arguments) -> name ^ "(" ^ (string_of_function_arguments arguments) ^ ")"
and
string_of_function_arguments = function
    |[] -> ""
    |t::[] -> string_of_expression t
    |t::q -> (string_of_expression t) ^ ", " ^ (string_of_function_arguments q)
;;

let rec string_of_function_parameters = function
    |[] -> ""
    |(var_t, name)::[] -> (string_of_type var_t) ^ " " ^ name
    |(var_t, name)::q -> (string_of_type var_t) ^ " " ^ name ^ ", " ^ (string_of_function_parameters q)
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
    |WriteChar expr -> "write_char(" ^ (string_of_expression expr) ^ ");"
    |Block(statements) ->
        "{\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n}"
    |Function(None, name, parameters, statements) ->
        "void " ^ name ^ "(" ^ (string_of_function_parameters parameters) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n}"
    |Function(Some(var_t, returned_expr), name, parameters, statements) ->
        (string_of_type var_t) ^ " " ^ name ^ "(" ^ (string_of_function_parameters parameters) ^ ") {\n\t" ^
        (global_replace (regexp "\n") "\n\t" (string_of_program statements)) ^
        "\n\treturn " ^ (string_of_expression returned_expr) ^ ";" ^
        "\n}"
    |CallProcedure(name, arguments) -> name ^ "(" ^ (string_of_function_arguments arguments) ^ ");"
and
string_of_program = function
    |[] -> ""
    |statement::[] -> string_of_statement statement
    |statement::program -> (string_of_statement statement) ^ "\n" ^ (string_of_program program)
;;

let print_type vt = print_string (string_of_type vt);;
let print_constant c = print_string (string_of_constant c);;
let print_expression e = print_string (string_of_expression e);;
let print_function_parameters params = print_string (string_of_function_parameters params);;
let print_statement s = print_string (string_of_statement s);;
let print_program p = print_string (string_of_program p);;

(* conversion from string to Ast *)
let program_of_lexbuf lexbuf = nt_program tokenize lexbuf;;
let program_of_string src = program_of_lexbuf (Lexing.from_string src);;

(* check functions *)
type environment_type =
    |BasicType of var_type
    |FunctionType of (var_type option) * (var_type list)

type environment = (string * environment_type) list
exception Bad_type of string

let type_of_var env name = List.assoc name env;;

let type_of_constant = function
    |IntConst _ -> Int
    |BoolConst _ -> Bool
    |CharConst _ -> Int
;;

let is_function = function
    |FunctionType _ -> true
    |_ -> false

(* check_function_parameters : (var_type * string) list -> unit *)
let check_function_parameters =
    let rec aux params = function
        |[] -> ()
        |(_, name)::q ->
            if List.exists (fun x -> x = name) params then
                raise (Bad_type (sprintf "parameter %s already defined." name))
            else
                aux (name::params) q
    in
    aux []
;;

(* check_function_arguments : string -> var_type list * var_type list -> unit *)
let rec check_function_arguments name = function
    |[], [] -> ()
    |[], _ -> raise (Bad_type (sprintf "too many arguments for function %s" name))
    |_, [] -> raise (Bad_type (sprintf "too few arguments for function %s" name))
    |t1::q1, t2::q2 when (t1 = t2) -> check_function_arguments name (q1, q2)
    |t1::q1, t2::q2 -> raise (Bad_type (sprintf "bad argument for function %s : expected %s, found %s" name (string_of_type t1) (string_of_type t2)))
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
                match (type_of_var env name) with
                |BasicType t -> t
                |FunctionType _ -> raise (Bad_type (sprintf "cannot use function %s as a value" name))
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
        |ReadChar -> Int
        |Call(name, arguments) ->
            (
            try
                match (type_of_var env name) with
                |BasicType _ -> raise (Bad_type (sprintf "cannot use variable %s as a function" name))
                |FunctionType(None, _) -> raise (Bad_type (sprintf "function %s returns void in an expression" name))
                |FunctionType(Some(returned_type), arguments_types) ->
                    let arguments_real_types = List.map (type_of_expression env) arguments in
                    check_function_arguments name (arguments_types, arguments_real_types);
                    returned_type
            with Not_found ->
                raise (Bad_type (sprintf "function %s is not defined" name))
            )
;;

let check_types =
    let rec aux env = function
        |[] -> ()
        |Define(var_t, name, expr)::q ->
            if List.mem_assoc name env then
                raise (Bad_type (sprintf "variable %s already defined." name))
            else if not(var_t = type_of_expression env expr) then
                raise (Bad_type (sprintf "expression of type %s cannot be assigned to variable %s of type %s." (string_of_type (type_of_expression env expr)) name (string_of_type var_t)));
            aux ((name, BasicType var_t)::env) q
        |Assign(name, expr)::q ->
            (
            try
                match (type_of_var env name) with
                |FunctionType _ -> raise (Bad_type (sprintf "%s is not a variable" name))
                |BasicType var_t ->
                    if not(var_t = type_of_expression env expr) then
                        raise (Bad_type (sprintf "expression of type %s cannot be assigned to variable %s of type %s." (string_of_type (type_of_expression env expr)) name (string_of_type var_t)))
            with Not_found ->
                raise (Bad_type (sprintf "variable %s is not defined." name))
            );
            aux env q
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
        |WriteChar(expr)::q ->
            if not(type_of_expression env expr = Int) then
                raise (Bad_type (sprintf "read_char is expected a int, not %s." (string_of_type (type_of_expression env expr))));
            aux env q
        |Block(statements)::q ->
            aux env statements;
            aux env q
        |Function(None, name, parameters, statements)::q ->
            check_function_parameters parameters;
            let function_env = (List.map (fun (var_t, name) -> (name, BasicType var_t)) parameters) @ (List.filter (fun (_, t) -> is_function t) env) in
            aux function_env statements;
            let new_env = (name, FunctionType(None, List.map (function (var_t, _) -> var_t) parameters))::env in
            aux new_env q
        |Function(Some(var_t, returned_expr), name, parameters, statements)::q ->
            check_function_parameters parameters;
            let function_env = (List.map (fun (var_t, name) -> (name, BasicType var_t)) parameters) @ (List.filter (fun (_, t) -> is_function t) env) in
            aux function_env (statements @ [Define(var_t, "_return", returned_expr)]);
            let new_env = (name, FunctionType(Some(var_t), List.map (function (var_t, _) -> var_t) parameters))::env in
            aux new_env q
        |CallProcedure(name, arguments)::q ->
            (
            try
                match (type_of_var env name) with
                |BasicType _ -> raise (Bad_type (sprintf "cannot use variable %s as a function" name))
                |FunctionType(_, arguments_types) ->
                    let arguments_real_types = List.map (type_of_expression env) arguments in
                    check_function_arguments name (arguments_types, arguments_real_types)
            with Not_found ->
                raise (Bad_type (sprintf "function %s is not defined" name))
            );
            aux env q
        in
        aux [];;
