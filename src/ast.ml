open Types
open Ast_parser
open Ast_lexer
open Printf
open Str

(* print functions *)
let rec string_of_type = function
    |Int -> "int"
    |Bool -> "bool"
    |Array t -> (string_of_type t) ^ "[]"

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
    |Xor(left, right) -> "(" ^ (string_of_expression left) ^ ") ^ (" ^ (string_of_expression right) ^ ")"
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
    |ArrayAccess(name, expr) -> name ^ "[" ^ (string_of_expression expr) ^ "]"
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
    |DefineEmptyArray(var_t, size, name) -> (string_of_type var_t) ^ " " ^ name ^ "[" ^ (string_of_int size) ^ "];"
    |DefineFullArray(var_t, name, expressions) -> (string_of_type var_t) ^ " " ^ name ^ "[] = {" ^ (string_of_function_arguments expressions) ^ "};"
    |DefineCharArray(name, value) -> "int " ^ name ^ "[] = \"" ^ value ^ "\";"
    |ArrayWrite(name, index, value) -> name ^ "[" ^ (string_of_expression index) ^ "] = " ^ (string_of_expression value) ^ ";"
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

let is_array = function
    |Array _ -> true
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
        |Xor(left, right) -> type_of_binary_expr "xor" Int Int left right
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
                |FunctionType(Some(returned_type), parameters_types) ->
                    let arguments_real_types = List.map (type_of_expression env) arguments in
                    check_function_arguments name (parameters_types, arguments_real_types);
                    returned_type
            with Not_found ->
                raise (Bad_type (sprintf "function %s is not defined" name))
            )
        |ArrayAccess(name, expr) ->
            (
            try
                match (type_of_var env name) with
                |BasicType(Array array_type) ->
                    if type_of_expression env expr != Int then
                        raise (Bad_type (sprintf "array subscript is not an integer"));
                    array_type
                |BasicType _ -> raise (Bad_type (sprintf "variable %s is not an array" name))
                |FunctionType _ -> raise (Bad_type (sprintf "cannot use function %s as an array" name))
            with Not_found ->
                raise (Bad_type (sprintf "variable %s is not defined" name))
            )
;;

let check_types =
    let rec aux env = function
        |[] -> ()
        |Define(var_t, name, expr)::q ->
            if is_array var_t then
                raise (Bad_type (sprintf "invalid initializer"));

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
                    if is_array var_t then
                        raise (Bad_type (sprintf "assignment to expression with array type"));

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
            if is_array var_t then
                raise (Bad_type (sprintf "function %s cannot return an array." name));

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
                |FunctionType(Some(_), _) -> raise (Bad_type (sprintf "function %s cannot be used as a procedure" name))
                |FunctionType(None, parameters_types) ->
                    let arguments_real_types = List.map (type_of_expression env) arguments in
                    check_function_arguments name (parameters_types, arguments_real_types)
            with Not_found ->
                raise (Bad_type (sprintf "function %s is not defined" name))
            );
            aux env q
        |DefineEmptyArray(var_t, size, name)::q ->
            if List.mem_assoc name env then
                raise (Bad_type (sprintf "variable %s already defined." name));
            if size < 0 then
                raise (Bad_type (sprintf "size of array %s is negative" name));
            if is_array var_t then
                raise (Bad_type "cannot define an array of arrays.");

            aux ((name, BasicType(Array var_t))::env) q
        |DefineFullArray(var_t, name, expressions)::q ->
            if List.mem_assoc name env then
                raise (Bad_type (sprintf "variable %s already defined." name));
            if is_array var_t then
                raise (Bad_type "cannot define an array of arrays.");

            let check_item expr =
                if var_t != type_of_expression env expr then
                    raise (Bad_type (sprintf "invalid conversion from %s to %s" (string_of_type (type_of_expression env expr)) (string_of_type var_t)))
            in

            List.iter check_item expressions;
            aux ((name, BasicType(Array var_t))::env) q
        |DefineCharArray(name, value)::q ->
            if List.mem_assoc name env then
                raise (Bad_type (sprintf "variable %s already defined." name));

            aux ((name, BasicType(Array Int))::env) q
        |ArrayWrite(name, index, value)::q ->
            (
            try
                match (type_of_var env name) with
                |BasicType (Array var_t) ->
                    if type_of_expression env index != Int then
                        raise (Bad_type (sprintf "array subscript is not an integer"));
                    if not(var_t = type_of_expression env value) then
                        raise (Bad_type (sprintf "invalid conversion from %s to %s" (string_of_type (type_of_expression env value)) (string_of_type var_t)))
                |BasicType _ -> raise (Bad_type (sprintf "%s is not an array" name))
                |FunctionType _ -> raise (Bad_type (sprintf "%s is not a variable" name))
            with Not_found ->
                raise (Bad_type (sprintf "variable %s is not defined." name))
            );
            aux env q
        in
        aux []
;;

(* rename_variable_expr : string -> string -> expression -> expression
 *
 * replace a variable name in a expression
 *)
let rename_variable_expr old_name new_name =
    let rec aux = function
        |Const cst -> Const cst
        |ReadChar -> ReadChar
        |Var name when (name = old_name) -> Var new_name
        |Var name -> Var name
        |Minus e -> Minus (aux e)
        |Not e -> Not (aux e)
        |Add(left, right) -> Add(aux left, aux right)
        |Sub(left, right) -> Sub(aux left, aux right)
        |Mul(left, right) -> Mul(aux left, aux right)
        |Div(left, right) -> Div(aux left, aux right)
        |Xor(left, right) -> Xor(aux left, aux right)
        |Inf(left, right)   -> Inf(aux left, aux right)
        |InfEq(left, right) -> InfEq(aux left, aux right)
        |Eq(left, right)    -> Eq(aux left, aux right)
        |NotEq(left, right) -> NotEq(aux left, aux right)
        |Sup(left, right)   -> Sup(aux left, aux right)
        |SupEq(left, right) -> SupEq(aux left, aux right)
        |And(left, right) -> And(aux left, aux right)
        |Or(left, right)  -> Or(aux left, aux right)
        |Call(name, arguments) -> Call(name, List.map aux arguments)
        |ArrayAccess(name, e) when (name = old_name) -> ArrayAccess(new_name, aux e)
        |ArrayAccess(name, e) -> ArrayAccess(name, aux e)
    in aux
;;

(* rename_variable_statement : string -> string -> statement -> statement
 *
 * replace a variable name in a statement, ignoring functions
 *)
let rec rename_variable_statement old_name new_name = function
    |Define(var_t, name, expr) when (name = old_name) -> Define(var_t, new_name, rename_variable_expr old_name new_name expr)
    |Define(var_t, name, expr) -> Define(var_t, name, rename_variable_expr old_name new_name expr)
    |Assign(name, expr) when (name = old_name) -> Assign(new_name, rename_variable_expr old_name new_name expr)
    |Assign(name, expr) -> Assign(name, rename_variable_expr old_name new_name expr)
    |If(cond, statements_true, statements_false) -> If(rename_variable_expr old_name new_name cond, rename_variable_prog old_name new_name statements_true, rename_variable_prog old_name new_name statements_false)
    |While(cond, statements) -> While(rename_variable_expr old_name new_name cond, rename_variable_prog old_name new_name statements)
    |For(init, cond, incr, statements) -> For(rename_variable_statement old_name new_name init, rename_variable_expr old_name new_name cond, rename_variable_statement old_name new_name incr, rename_variable_prog old_name new_name statements)
    |WriteChar(expr) -> WriteChar(rename_variable_expr old_name new_name expr)
    |Block(statements) -> Block(rename_variable_prog old_name new_name statements)
    |Function(return, name, parameters, statements) -> Function(return, name, parameters, statements)
    |CallProcedure(name, arguments) -> CallProcedure(name, List.map (rename_variable_expr old_name new_name) arguments)
    |DefineEmptyArray(var_t, size, name) when (name = old_name) -> DefineEmptyArray(var_t, size, new_name)
    |DefineEmptyArray(var_t, size, name) -> DefineEmptyArray(var_t, size, name)
    |DefineFullArray(var_t, name, expressions) when(name = old_name) -> DefineFullArray(var_t, new_name, List.map (rename_variable_expr old_name new_name) expressions)
    |DefineFullArray(var_t, name, expressions) -> DefineFullArray(var_t, name, List.map (rename_variable_expr old_name new_name) expressions)
    |DefineCharArray(name, value) when(name = old_name) -> DefineCharArray(new_name, value)
    |DefineCharArray(name, value) -> DefineCharArray(name, value)
    |ArrayWrite(name, index, value) when (name = old_name) -> ArrayWrite(new_name, rename_variable_expr old_name new_name index, rename_variable_expr old_name new_name value)
    |ArrayWrite(name, index, value) -> ArrayWrite(name, rename_variable_expr old_name new_name index, rename_variable_expr old_name new_name value)

(* rename_variable_prog : string -> string -> program -> program
 *
 * replace a variable name in a program, ignoring functions
 *)
and rename_variable_prog old_name new_name p = List.map (rename_variable_statement old_name new_name) p
;;

(* default_value : var_type -> constant *)
let default_value = function
    |Int -> IntConst 0
    |Bool -> BoolConst(false)
    |Array _ -> failwith "unreachable"
;;

type function_value = (var_type * expression) option * function_parameters * statement list
type inline_environment = (string * function_value) list

(*
 * inline : program -> program
 *
 * remove all functions, assuming that the program is well typed
 *)
let inline =
    (* for temporary variables in expressions *)
    let tmp_counter = ref 0 in

    let get_tmp_var () =
        incr tmp_counter;
        sprintf "%d_tmp" (!tmp_counter)
    in

    (* for local variables inside functions *)
    let call_counter = ref 0 in

    let name_local_var call_id name = sprintf "%d_loc_%s" call_id name in

    (* inline_expression : inline_environment -> expression -> program * expression *)
    let rec inline_expression env expr =
        let inline_unary_expr expr construct =
            let pre_expr, new_expr = inline_expression env expr in
            pre_expr, construct new_expr
        in
        let inline_binary_expr left right construct =
            let pre_left, new_left = inline_expression env left in
            let pre_right, new_right = inline_expression env right in
            pre_left @ pre_right, construct new_left new_right
        in
        match expr with
        |Const cst -> [], expr
        |ReadChar -> [], expr
        |Var name -> [], Var name
        |Minus e -> inline_unary_expr e (fun x -> Minus x)
        |Not e -> inline_unary_expr e (fun x -> Not e)
        |Add(left, right) -> inline_binary_expr left right (fun x y -> Add(x, y))
        |Sub(left, right) -> inline_binary_expr left right (fun x y -> Sub(x, y))
        |Mul(left, right) -> inline_binary_expr left right (fun x y -> Mul(x, y))
        |Div(left, right) -> inline_binary_expr left right (fun x y -> Div(x, y))
        |Xor(left, right) -> inline_binary_expr left right (fun x y -> Xor(x, y))
        |Inf(left, right)   -> inline_binary_expr left right (fun x y -> Inf(x, y))
        |InfEq(left, right) -> inline_binary_expr left right (fun x y -> InfEq(x, y))
        |Eq(left, right)    -> inline_binary_expr left right (fun x y -> Eq(x, y))
        |NotEq(left, right) -> inline_binary_expr left right (fun x y -> NotEq(x, y))
        |Sup(left, right)   -> inline_binary_expr left right (fun x y -> Sup(x, y))
        |SupEq(left, right) -> inline_binary_expr left right (fun x y -> SupEq(x, y))
        |And(left, right) -> inline_binary_expr left right (fun x y -> And(x, y))
        |Or(left, right)  -> inline_binary_expr left right (fun x y -> Or(x, y))
        |Call(name, arguments) ->
            (
            try
                match (List.assoc name env) with
                |None, _, _ -> raise (Bad_type (sprintf "function %s returns void in an expression" name))
                |Some(returned_type, returned_expr), parameters_types, statements ->
                    (* update call counter *)
                    incr call_counter;
                    let call_id = !call_counter in

                    (* build initialization statements : `type argument = expr;` *)
                    let pre_function = function_init_arguments call_id env (parameters_types, arguments) in

                    (* put the result in a temporary var *)
                    let tmp = get_tmp_var () in
                    let function_body = statements @ [Define(returned_type, tmp, returned_expr)] in

                    (* replace argument variable names *)
                    let function_body = List.fold_left (fun prog (_, name) -> rename_variable_prog name (name_local_var call_id name) prog) function_body parameters_types in

                    (* inline function body *)
                    let function_body = inline_program call_id env function_body in

                    pre_function @ function_body, Var tmp

            with Not_found ->
                raise (Bad_type (sprintf "function %s is not defined" name))
            )
        |ArrayAccess(name, e) ->
            let pre_e, new_e = inline_expression env e in
            pre_e, ArrayAccess(name, new_e)

    (* function_init_arguments : int -> inline_environment -> ((var_type * string) list) * (expression list) *)
    and function_init_arguments call_id env = function
        |[], [] -> []
        |[], _ -> raise (Bad_type (sprintf "too many arguments for call %d" call_id))
        |_, [] -> raise (Bad_type (sprintf "too few arguments for call %d" call_id))
        |(var_t, name)::q1, expr::q2 ->
            let pre_expr, new_expr = inline_expression env expr in
            pre_expr @ [Define(var_t, name_local_var call_id name, new_expr)] @ (function_init_arguments call_id env (q1, q2))

    (* inline_expression_list : int -> inline_environment -> expression list -> program * expression list *)
    and inline_expression_list call_id env = function
        |[] -> [], []
        |expr::q ->
            let pre, expressions = inline_expression_list call_id env q in
            let pre_expr, new_expr = inline_expression env expr in
            pre_expr @ pre, new_expr::expressions

    (* inline_program : int -> inline_environment -> program -> program *)
    and inline_program call_id env = function
        |[] -> []
        |Define(var_t, name, expr)::q ->
            let new_name = (match name.[0] with
                |'0'..'9' -> name (* don't replace name *)
                |_ -> name_local_var call_id name
            ) in
            let pre_expr, new_expr = inline_expression env expr in
            let inlined_q = inline_program call_id env (rename_variable_prog name new_name q) in

            if pre_expr = [] then
                Define(var_t, new_name, new_expr)::inlined_q
            else
                Define(var_t, new_name, Const(default_value var_t))::Block(pre_expr @ [Assign(new_name, new_expr)])::inlined_q
        |Assign(name, expr)::q ->
            let pre_expr, new_expr = inline_expression env expr in
            let inlined_q = inline_program call_id env q in

            if pre_expr = [] then
                Assign(name, new_expr)::inlined_q
            else
                Block(pre_expr @ [Assign(name, new_expr)])::inlined_q
        |If(cond, statements_true, statements_false)::q ->
            let pre_cond, new_cond = inline_expression env cond in
            let inlined_statements_true = inline_program call_id env statements_true in
            let inlined_statements_false = inline_program call_id env statements_false in
            let inlined_q = inline_program call_id env q in

            if pre_cond = [] then
                If(new_cond, inlined_statements_true, inlined_statements_false)::inlined_q
            else
                Block(pre_cond @ [If(new_cond, inlined_statements_true, inlined_statements_false)])::inlined_q
        |While(cond, statements)::q ->
            let pre_cond, new_cond = inline_expression env cond in
            let inlined_statements = inline_program call_id env statements in
            let inlined_q = inline_program call_id env q in

            if pre_cond = [] then
                While(new_cond, inlined_statements)::inlined_q
            else
                (* replace define that are not in a block (if, while, for, block) *)
                let replace_define = function
                    |Define(var_t, name, expr) -> Assign(name, expr)
                    |x -> x
                in
                let pre_cond_without_define = List.map replace_define pre_cond in
                Block(pre_cond @ [While(new_cond, inlined_statements @ pre_cond_without_define)])::inlined_q
        |For(init, cond, incr, statements)::q ->
            inline_program call_id env (Block([init; While(cond, statements @ [incr])])::q)
        |WriteChar(expr)::q ->
            let pre_expr, new_expr = inline_expression env expr in
            let inlined_q = inline_program call_id env q in

            if pre_expr = [] then
                WriteChar(new_expr)::inlined_q
            else
                Block(pre_expr @ [WriteChar(new_expr)])::inlined_q
        |Block(statements)::q ->
            let inlined_statements = inline_program call_id env statements in
            let inlined_q = inline_program call_id env q in

            Block(inlined_statements)::inlined_q
        |Function(return, name, parameters, statements)::q ->
            inline_program call_id ((name, (return, parameters, statements))::env) q
        |CallProcedure(name, arguments)::q ->
            (
            try
                match (List.assoc name env) with
                |Some(_), _, _ -> raise (Bad_type (sprintf "function %s cannot be used as a procedure" name))
                |None, parameters_types, statements ->
                    (* update call counter *)
                    incr call_counter;
                    let new_call_id = !call_counter in

                    (* build initialization statements : `type argument = expr;` *)
                    let pre_function = function_init_arguments new_call_id env (parameters_types, arguments) in

                    (* replace argument variable names *)
                    let function_body = List.fold_left (fun prog (_, name) -> rename_variable_prog name (name_local_var new_call_id name) prog) statements parameters_types in

                    (* inline function body *)
                    let function_body = inline_program new_call_id env function_body in

                    Block(pre_function @ function_body)::(inline_program call_id env q)
            with Not_found ->
                raise (Bad_type (sprintf "function %s is not defined" name))
            )
        |DefineEmptyArray(var_t, size, name)::q ->
            let new_name = (match name.[0] with
                |'0'..'9' -> name (* don't replace name *)
                |_ -> name_local_var call_id name
            ) in
            let inlined_q = inline_program call_id env (rename_variable_prog name new_name q) in
            DefineEmptyArray(var_t, size, new_name)::inlined_q
        |DefineFullArray(var_t, name, expressions)::q ->
            let new_name = (match name.[0] with
                |'0'..'9' -> name (* don't replace name *)
                |_ -> name_local_var call_id name
            ) in
            let pre_expressions, new_expressions = inline_expression_list call_id env expressions in
            let inlined_q = inline_program call_id env (rename_variable_prog name new_name q) in
            pre_expressions @ (DefineFullArray(var_t, new_name, new_expressions)::inlined_q)
        |DefineCharArray(name, value)::q ->
            let new_name = (match name.[0] with
                |'0'..'9' -> name (* don't replace name *)
                |_ -> name_local_var call_id name
            ) in
            let inlined_q = inline_program call_id env (rename_variable_prog name new_name q) in
            DefineCharArray(new_name, value)::inlined_q
        |ArrayWrite(name, index, value)::q ->
            let pre_index, new_index = inline_expression env index in
            let pre_value, new_value = inline_expression env value in
            let inlined_q = inline_program call_id env q in

            if pre_index = [] && pre_value = [] then
                ArrayWrite(name, new_index, new_value)::inlined_q
            else
                Block(pre_index @ pre_value @ [ArrayWrite(name, new_index, new_value)])::inlined_q

    in
    inline_program (!call_counter) []
