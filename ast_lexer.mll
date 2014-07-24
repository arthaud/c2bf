{
    open Ast_parser
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule tokenize = parse
    | [' ' '\t' '\n' '\r']                    { tokenize lexbuf }
    | "int"                                   { TInt }
    | "bool"                                  { TBool }
    | '('                                     { TLeftPar }
    | ')'                                     { TRightPar }
    | '{'                                     { TLeftBrace }
    | '}'                                     { TRightBrace }
    | '+'                                     { TPlus }
    | '-'                                     { TMinus }
    | '*'                                     { TMul }
    | '/'                                     { TDiv }
    | '<'                                     { TInf }
    | "<="                                    { TInfEq }
    | "=="                                    { TEq }
    | "!="                                    { TNotEq }
    | '>'                                     { TSup }
    | ">="                                    { TSupEq }
    | "&&"                                    { TAnd }
    | "||"                                    { TOr }
    | '!'                                     { TNot }
    | ';'                                     { TSemicolon }
    | ','                                     { TComma }
    | '='                                     { TAssign }
    | "if"                                    { TIf }
    | "else"                                  { TElse }
    | "while"                                 { TWhile }
    | "for"                                   { TFor }
    | (digit+ as num)                         { TIntConst(int_of_string num) }
    | "true"                                  { TBoolConst(true) }
    | "false"                                 { TBoolConst(false) }
    | ident as var_name                       { TVar(var_name) }
    | eof                                     { TEOF }
