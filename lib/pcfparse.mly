%{
open Pcfast ;;

let rec body params expr = match params with
  | [] -> expr
  | p :: prms -> E_Fun (p, body prms expr)
;;
%}

%token <int> INT
%token <string> IDENT
%token <string> TYVAR
%token TRUE FALSE
%token <string> STRING
%token PLUS MINUS MULT DIV EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL POINT POW
%token LPAR RPAR SEMISEMI COMMA COLON
%token LET REC LETREC IN FUN ARROW
%token IF THEN ELSE

%left COMMA                       /* , types */
%right ARROW            /* -> expressions de types */
%left EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%left PLUS MINUS
%left MULT DIV

%start main
%type <Pcfast.sentence> main

%%

main:
| expr SEMISEMI { S_Expr $1 }
| topdef SEMISEMI { $1 }
;

topdef:
| LETREC IDENT seqident EQUAL expr    { S_Letrec ($2, (body $3 $5)) }
| LET REC IDENT seqident EQUAL expr   { S_Letrec ($3, (body $4 $6)) }
| LET IDENT seqident EQUAL expr       { S_Let ($2, (body $3 $5)) }
;

expr:
| LETREC IDENT seqident EQUAL expr IN expr
    { E_Letrec ($2, (body $3 $5), $7) }
| LET REC IDENT seqident EQUAL expr IN expr
    { E_Letrec ($3, (body $4 $6), $8) }
| LET IDENT seqident EQUAL expr IN expr       { E_Let ($2, (body $3 $5) , $7) }
| FUN IDENT ARROW expr                        { E_Fun ($2, $4) }
| IF expr THEN expr ELSE expr                 { E_If ($2, $4, $6) }
| arith_expr                                  { $1 }
| LPAR expr COMMA expr RPAR                   { E_Pair ($2, $4) }
| LPAR expr COLON type_anot RPAR              { E_Tyannot ($2, $4) }
;

arith_expr:
  application                        { $1 }
| arith_expr EQUAL arith_expr        { E_Binop ("=", $1, $3) }
| arith_expr GREATER arith_expr      { E_Binop (">", $1, $3) }
| arith_expr GREATEREQUAL arith_expr { E_Binop (">=", $1, $3) }
| arith_expr SMALLER arith_expr      { E_Binop ("<", $1, $3) }
| arith_expr SMALLEREQUAL arith_expr { E_Binop ("<=", $1, $3) }
| arith_expr PLUS arith_expr         { E_Binop ("+", $1, $3) }
| arith_expr MINUS arith_expr        { E_Binop ("-", $1, $3) }
| arith_expr MULT arith_expr         { E_Binop ("*", $1, $3) }
| arith_expr DIV arith_expr          { E_Binop ("/", $1, $3) }
;

/* Attention : on considere ci-dessous que MINUS atom est dans la categorie
 * des applications. Cela permet de traiter
 *                        n - 1
 *  comme une soustraction binaire, et
 *                        f (- 1)
 * comme l'application de f a l'oppose de 1.
 */

application:
  atom             { $1 }
| MINUS atom       { E_Monop("-", $2) }
| application atom { E_App($1, $2) }
;

atom:
  INT            { E_Int $1 }
| TRUE           { E_Bool true }
| FALSE          { E_Bool false }
| STRING         { E_String $1 }
| IDENT          { E_Ident $1 }
| LPAR expr RPAR { $2 }
;

seqident:
  IDENT seqident  { $1 :: $2 }
| /* rien */      { [ ] }
;

type_anot:
| TYVAR                               { TE_Var $1 }
| IDENT core_unit                     { TE_Basic ($1,$2) }
| LPAR type_anot MULT type_anot RPAR  { TE_Pair ($2, $4) }
| type_anot ARROW type_anot           { TE_Fun ($1, $3) }
| LPAR type_anot RPAR                 { $2 }
;

core_unit:
| SMALLER GREATER        { UE_UOne } //unit
| SMALLER u_expr GREATER { $2 }
;

u_expr:
| IDENT               { UE_Base $1 }
| u_expr POINT u_expr { UE_Mul ($1, $3) }
| u_expr DIV u_expr   { UE_Div ($1, $3) }
| u_expr POW INT      { UE_Pow ($1, $3) } //uniquement puissances entières reconnues
| LPAR u_expr RPAR    { $2 }