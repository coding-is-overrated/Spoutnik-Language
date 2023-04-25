%{
  open Pcfast;;

  let rec body params expr = match params with
  | [] -> expr
  | p :: prms -> Fun(p, body prms expr)
  ;;
%}

%token <int> INT
%token <string> IDENT
%token TRUE FALSE
%token <string> STRING
%token PLUS MINUS MULT DIV EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%token LPAR RPAR SEMISEMI
%token LET REC LETREC IN FUN ARROW
%token IF THEN ELSE
%left EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%left PLUS MINUS
%left MULT DIV

%start main
%type <Pcfast.expr> main

%%

main: expr SEMISEMI { $1 }
;

expr:
  LETREC IDENT seqident EQUAL expr IN expr    { Letrec($2, (body $3 $5), $7) }
| LET REC IDENT seqident EQUAL expr IN expr   { Letrec($3, (body $4 $6), $8) }
| LET IDENT seqident EQUAL expr IN expr       { Let($2, (body $3 $5) , $7) }
| FUN IDENT ARROW expr                        { Fun($2, $4) }
| IF expr THEN expr ELSE expr                 { If($2, $4, $6) }
| arith_expr                                  { $1 }
;

arith_expr:
  application                        { $1 }
| arith_expr EQUAL arith_expr        { Binop("=", $1, $3) }
| arith_expr GREATER arith_expr      { Binop(">", $1, $3) }
| arith_expr GREATEREQUAL arith_expr { Binop(">=", $1, $3) }
| arith_expr SMALLER arith_expr      { Binop("<", $1, $3) }
| arith_expr SMALLEREQUAL arith_expr { Binop("<=", $1, $3) }
| arith_expr PLUS arith_expr         { Binop("+", $1, $3) }
| arith_expr MINUS arith_expr        { Binop("-", $1, $3) }
| arith_expr MULT arith_expr         { Binop("*", $1, $3) }
| arith_expr DIV arith_expr          { Binop("/", $1, $3) }
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
| MINUS atom       { Monop("-", $2) }
| application atom { App($1, $2) }
;

atom:
  INT            { Int($1) }
| TRUE           { Bool(true) }
| FALSE          { Bool(false) }
| STRING         { String($1) }
| IDENT          { Ident($1) }
| LPAR expr RPAR { $2 }
;

seqident:
  IDENT seqident  { $1 :: $2 }
| /* rien */      { [ ] }
;
