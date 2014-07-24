%{
    open Types
%}

%token <string> TVar
%token <int> TIntConst
%token <bool> TBoolConst
%token TInt TBool
%token TLeftPar TRightPar TLeftBrace, TRightBrace
%token TPlus TMinus TMul TDiv
%token TInf TInfEq TEq TNotEq TSup TSupEq TAnd TOr TNot
%token TSemicolon TComma TAssign
%token TIf TElse TWhile TFor
%token TEOF

%start nt_program
%type <Types.program> nt_program

%left TOr
%left TAnd
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

nt_statement : 
    | TInt TVar TAssign nt_expression TSemicolon { Define(Int, $2, $4) }
    | TBool TVar TAssign nt_expression TSemicolon { Define(Bool, $2, $4) }
    | TVar TAssign nt_expression TSemicolon { Assign($1, $3) }
    | TIf TLeftPar nt_expression TRightPar TLeftBrace nt_statements TRightBrace TElse TLeftBrace nt_statements TRightBrace { If($3, $6, $10) }
    | TIf TLeftPar nt_expression TRightPar TLeftBrace nt_statements TRightBrace { If($3, $6, []) }
    | TWhile TLeftPar nt_expression TRightPar TLeftBrace nt_statements TRightBrace { While($3, $6) }
    | TFor TLeftPar nt_statement TComma nt_expression TComma nt_statement TRightPar TLeftBrace nt_statements TRightBrace { For($3, $5, $7, $10) }
;

nt_expression :
    | TLeftPar nt_expression TRightPar { $2 }
    | TVar { Var($1) }
    | TIntConst { Const(IntConst($1)) }
    | TBoolConst { Const(BoolConst($1)) }
    | TMinus nt_expression %prec TNeg { Minus($2) }
    | nt_expression TPlus nt_expression { Add($1, $3) }
    | nt_expression TMinus nt_expression { Sub($1, $3) }
    | nt_expression TMul nt_expression { Mul($1, $3) }
    | nt_expression TDiv nt_expression { Div($1, $3) }
    | TNot nt_expression { Not($2) }
    | nt_expression TOr nt_expression { Or($1, $3) }
    | nt_expression TAnd nt_expression { And($1, $3) }
    | nt_expression TInf nt_expression { Inf($1, $3) }
    | nt_expression TInfEq nt_expression { InfEq($1, $3) }
    | nt_expression TEq nt_expression { Eq($1, $3) }
    | nt_expression TNotEq nt_expression { NotEq($1, $3) }
    | nt_expression TSup nt_expression { Sup($1, $3) }
    | nt_expression TSupEq nt_expression { SupEq($1, $3) }
;
