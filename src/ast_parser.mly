%{
    open Types
%}

%token <string> TVar
%token <int> TIntConst
%token <bool> TBoolConst
%token <char> TCharConst
%token <string> TStringConst
%token TVoid TInt TBool
%token TLeftPar TRightPar TLeftBrace TRightBrace TOpenBracket TCloseBracket
%token TPlus TMinus TMul TDiv TXor
%token TInf TInfEq TEq TNotEq TSup TSupEq TAnd TOr TNot
%token TSemicolon TComma TAssign TReturn
%token TIf TElse TWhile TFor
%token TReadChar TWriteChar
%token TEOF

%start nt_program
%type <Types.program> nt_program

%left TOr
%left TAnd
%left TXor
%left TInf TInfEq TEq TNotEq TSup TSupEq
%left TPlus TMinus
%left TMul TDiv
%left TNeg TNot

%%

nt_program :
    | TEOF { [] }
    | nt_statement nt_program { $1::$2 }
;

nt_statements :
    | { [] }
    | nt_statement nt_statements { $1::$2 }
;

nt_basic_type :
    | TInt { Int }
    | TBool { Bool }
;

nt_type :
    | nt_basic_type { $1 }
    | nt_basic_type TOpenBracket TCloseBracket { Array($1) }
;

nt_function_parameters :
    | { [] }
    | nt_type TVar nt_function_parameters_list { ($1, $2)::$3 }
;

nt_function_parameters_list :
    | { [] }
    | TComma nt_type TVar nt_function_parameters_list { ($2, $3)::$4 }
;

nt_function_arguments :
    | { [] }
    | nt_expression nt_function_arguments_list { $1::$2 }
;

nt_function_arguments_list :
    | { [] }
    | TComma nt_expression nt_function_arguments_list { $2::$3 }
;

nt_statement : 
    | nt_type TVar TAssign nt_expression TSemicolon { Define($1, $2, $4) }
    | TVar TAssign nt_expression TSemicolon { Assign($1, $3) }
    | TIf TLeftPar nt_expression TRightPar TLeftBrace nt_statements TRightBrace TElse TLeftBrace nt_statements TRightBrace { If($3, $6, $10) }
    | TIf TLeftPar nt_expression TRightPar TLeftBrace nt_statements TRightBrace { If($3, $6, []) }
    | TWhile TLeftPar nt_expression TRightPar TLeftBrace nt_statements TRightBrace { While($3, $6) }
    | TFor TLeftPar nt_statement TComma nt_expression TComma nt_statement TRightPar TLeftBrace nt_statements TRightBrace { For($3, $5, $7, $10) }
    | TWriteChar TLeftPar nt_expression TRightPar TSemicolon { WriteChar($3) }
    | TLeftBrace nt_statements TRightBrace { Block($2) }
    | nt_type TVar TLeftPar nt_function_parameters TRightPar TLeftBrace nt_statements TReturn nt_expression TSemicolon TRightBrace { Function(Some ($1, $9), $2, $4, $7) }
    | TVoid TVar TLeftPar nt_function_parameters TRightPar TLeftBrace nt_statements TRightBrace { Function(None, $2, $4, $7) }
    | TVar TLeftPar nt_function_arguments TRightPar TSemicolon { CallProcedure($1, $3) }
    | nt_type TVar TOpenBracket TIntConst TCloseBracket TSemicolon { DefineEmptyArray($1, $4, $2) }
    | nt_type TVar TOpenBracket TCloseBracket TAssign TLeftBrace nt_function_arguments TRightBrace TSemicolon { DefineFullArray($1, $2, $7) }
    | nt_type TVar TOpenBracket TCloseBracket TAssign TStringConst TSemicolon { DefineCharArray($2, $6) }
    | TVar TOpenBracket nt_expression TCloseBracket TAssign nt_expression TSemicolon { ArrayWrite($1, $3, $6) }
;

nt_expression :
    | TLeftPar nt_expression TRightPar { $2 }
    | TVar { Var($1) }
    | TIntConst { Const(IntConst($1)) }
    | TBoolConst { Const(BoolConst($1)) }
    | TCharConst { Const(CharConst($1)) }
    | TMinus nt_expression %prec TNeg { Minus($2) }
    | nt_expression TPlus nt_expression { Add($1, $3) }
    | nt_expression TMinus nt_expression { Sub($1, $3) }
    | nt_expression TMul nt_expression { Mul($1, $3) }
    | nt_expression TDiv nt_expression { Div($1, $3) }
    | nt_expression TXor nt_expression { Xor($1, $3) }
    | TNot nt_expression { Not($2) }
    | nt_expression TOr nt_expression { Or($1, $3) }
    | nt_expression TAnd nt_expression { And($1, $3) }
    | nt_expression TInf nt_expression { Inf($1, $3) }
    | nt_expression TInfEq nt_expression { InfEq($1, $3) }
    | nt_expression TEq nt_expression { Eq($1, $3) }
    | nt_expression TNotEq nt_expression { NotEq($1, $3) }
    | nt_expression TSup nt_expression { Sup($1, $3) }
    | nt_expression TSupEq nt_expression { SupEq($1, $3) }
    | TReadChar TLeftPar TRightPar { ReadChar }
    | TVar TLeftPar nt_function_arguments TRightPar { Call($1, $3) }
    | TVar TOpenBracket nt_expression TCloseBracket { ArrayAccess($1, $3) }
;
