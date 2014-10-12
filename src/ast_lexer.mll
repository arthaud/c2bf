{
    open Ast_parser
}

let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule tokenize = parse
    | [' ' '\t' '\n' '\r']                    { tokenize lexbuf }
    | "//"                                    { inline_comment lexbuf }
    | "/*"                                    { multiline_comment 1 lexbuf }
    | "void"                                  { TVoid }
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
    | "return"                                { TReturn }
    | (digit+ as num)                         { TIntConst(int_of_string num) }
    | "true"                                  { TBoolConst(true) }
    | "false"                                 { TBoolConst(false) }
    | '\'' (_ as c) '\''                      { TCharConst(c) }
    | "'\\t'"                                 { TCharConst('\t') }
    | "'\\n'"                                 { TCharConst('\n') }
    | "'\\r'"                                 { TCharConst('\r') }
    | "read_char"                             { TReadChar }
    | "write_char"                            { TWriteChar }
    | ident as var_name                       { TVar(var_name) }
    | eof                                     { TEOF }
and
inline_comment = parse
    | '\n'                                    { tokenize lexbuf }
    | eof                                     { TEOF }
    | _                                       { inline_comment lexbuf }
and
multiline_comment depth = parse
    | "*/"                                    { if depth = 1 then tokenize lexbuf else multiline_comment (depth - 1) lexbuf }
    | "/*"                                    { multiline_comment (depth + 1) lexbuf }
    | eof                                     { TEOF }
    | _                                       { multiline_comment depth lexbuf }
