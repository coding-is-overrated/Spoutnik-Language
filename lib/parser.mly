%{
open Ast ;;

type arg = Raw of string | Annotated of string * tyexpr

let rec body params ret_anot expr = match params with
  | [] -> (match ret_anot with | None -> expr | Some t -> E_Tyannot (expr, t))
  | Raw p :: prms -> E_Fun (p, body prms ret_anot expr)
  | Annotated (p, ty) :: prms -> E_Fun (p,
    E_Let (p, 
      E_Tyannot (E_Ident p, ty),
      body prms ret_anot expr
    ))

let body_rec f_name params ret_anot expr = 
let te_var () = let t = Types.type_var () in TE_Var (Types.get_tvar_string t) in
match ret_anot with
| None -> failwith "recursive definitions must have a return type annotation"
| Some t -> match params with
  | [] -> failwith "recursive definitions must be functional"
  | Raw p :: prms -> E_Fun (p,
    E_Let (f_name,
      E_Tyannot (E_Ident f_name, TE_Fun (te_var (), t)),
      body prms ret_anot expr
    ))
  | Annotated (p, ty) :: prms -> E_Fun (p,
    E_Let (f_name,
      E_Tyannot (E_Ident f_name, TE_Fun (te_var (), t)),
      E_Let (p,
        E_Tyannot (E_Ident p, ty),
        body prms ret_anot expr
      )
    ))

%}

%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> TYVAR
%token TRUE FALSE
%token <string> STRING
%token PLUS MINUS MULT DIV EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL POINT POW
%token LPAR RPAR SEMISEMI COMMA COLON UNDERSCORE
%token LET REC LETREC IN FUN ARROW
%token IF THEN ELSE

%left COMMA                       /* , types */
%right ARROW            /* -> expressions de types */
%left EQUAL GREATER SMALLER GREATEREQUAL SMALLEREQUAL
%left PLUS MINUS
%left MULT DIV

%start main
%type <Ast.sentence> main

%%

main:
| expr SEMISEMI { S_Expr $1 }
| topdef SEMISEMI { $1 }
;

topdef:
| LETREC IDENT seqident return_anot EQUAL expr    { S_Letrec ($2, (body_rec $2 $3 $4 $6)) }
| LET REC IDENT seqident return_anot EQUAL expr   { S_Letrec ($3, (body_rec $3 $4 $5 $7)) }
| LET IDENT seqident return_anot EQUAL expr       { S_Let ($2, (body $3 $4 $6)) }
;

expr:
| LETREC IDENT seqident return_anot EQUAL expr IN expr    { E_Letrec ($2, (body_rec $2 $3 $4 $6), $8) }
| LET REC IDENT seqident return_anot EQUAL expr IN expr   { E_Letrec ($3, (body_rec $3 $4 $5 $7), $9) }
| LET IDENT seqident return_anot EQUAL expr IN expr       { E_Let ($2, (body $3 $4 $6), $8) }
| FUN IDENT ARROW expr                        { E_Fun ($2, $4) }
| IF expr THEN expr ELSE expr                 { E_If ($2, $4, $6) }
| arith_expr                                  { $1 }
| LPAR expr COMMA expr RPAR                   { E_Pair ($2, $4) }
| LPAR expr COLON type_anot RPAR              { E_Tyannot ($2, $4) }
;

return_anot:
| /* empty */ { None }
| COLON type_anot { Some $2 }

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
| FLOAT          { E_Float $1 }
| TRUE           { E_Bool true }
| FALSE          { E_Bool false }
| STRING         { E_String $1 }
| IDENT          { E_Ident $1 }
| LPAR expr RPAR { $2 }
;

seqident:
| IDENT seqident  { Raw $1 :: $2 }
| LPAR IDENT COLON type_anot RPAR seqident { Annotated ($2, $4) :: $6 }
| /* rien */      { [ ] }
;

type_anot:
| TYVAR                               { TE_Var $1 }
| IDENT core_unit                     { TE_Basic ($1,$2) }
| LPAR type_anot MULT type_anot RPAR  { TE_Pair ($2, $4) }
| LPAR type_anot ARROW type_anot RPAR { TE_Fun ($2, $4) }
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