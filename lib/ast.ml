(** Ce fichier contient la définition du type OCaml des arbres de
    syntaxe abstraite du langage, ainsi qu'un imprimeur des phrases
    du langage.
*)

type u_expr =
  | UE_UOne
  | UE_Base of string
  | UE_Mul of (u_expr * u_expr)
  | UE_Div of (u_expr * u_expr)
  | UE_Pow of (u_expr * int)

type tyexpr =
  | TE_Var of string
  | TE_Basic of (string * u_expr)
  | TE_Pair of (tyexpr * tyexpr)
  | TE_Fun of (tyexpr * tyexpr)

type expr =
  | E_Int of int
  | E_Float of float
  | E_Bool of bool
  | E_String of string
  | E_Ident of string
  | E_App of expr * expr
  | E_Monop of string * expr
  | E_Binop of string * expr * expr
  | E_If of expr * expr * expr
  | E_Fun of string * expr
  | E_Let of string * expr * expr
  | E_Letrec of string * expr * expr
  | E_Pair of (expr * expr)
  | E_Tyannot of (expr * tyexpr)

type sentence =
  | S_Expr of expr
  | S_Let of string * expr
  | S_Letrec of string * expr

open Printf

let rec un_body params = function
  | E_Fun (p, e) -> un_body (p :: params) e
  | e -> (params, e)

let rec print_uexpr oc = function
  | UE_UOne -> fprintf oc "1"
  | UE_Base u -> fprintf oc "%s" u
  | UE_Mul (ue1, ue2) -> fprintf oc "%a.%a" print_uexpr ue1 print_uexpr ue2
  | UE_Div (ue1, ue2) -> fprintf oc "%a/%a" print_uexpr ue1 print_uexpr ue2
  | UE_Pow (ue1, i) -> fprintf oc "%a^%i" print_uexpr ue1 i

let rec print_texpr oc = function
  | TE_Var vn -> fprintf oc "'%s" vn
  | TE_Basic (n, u) -> fprintf oc "%s <%a>" n print_uexpr u
  | TE_Pair (te1, te2) -> fprintf oc "(%a, %a)" print_texpr te1 print_texpr te2
  | TE_Fun (te1, te2) -> fprintf oc "(%a -> %a)" print_texpr te1 print_texpr te2

let rec print_expr oc = function
  | E_Int n -> fprintf oc "%d" n
  | E_Float f -> fprintf oc "%F" f
  | E_Bool b -> fprintf oc "%s" (if b then "true" else "false")
  | E_Ident s -> fprintf oc "%s" s
  | E_String s -> fprintf oc "%S" s
  | E_App (e1, e2) -> fprintf oc "(%a %a)" print_expr e1 print_expr e2
  | E_Let (f, e1, e2) ->
      let params, e = un_body [] e1 in
      fprintf oc "(let %s %a= %a in %a)" f
        (fun oc -> List.iter (fun s -> fprintf oc "%s " s))
        params print_expr e print_expr e2
  | E_Letrec (f, e1, e2) ->
      let params, e = un_body [] e1 in
      fprintf oc "(letrec %s %a= %a in %a)" f
        (fun oc -> List.iter (fun s -> fprintf oc "%s " s))
        params print_expr e print_expr e2
  | E_Fun (x, e) -> fprintf oc "(fun %s -> %a)" x print_expr e
  | E_If (test, e1, e2) ->
      fprintf oc "(if %a then %a else %a)" print_expr test print_expr e1
        print_expr e2
  | E_Binop (op, e1, e2) ->
      fprintf oc "(%a %s %a)" print_expr e1 op print_expr e2
  | E_Monop (op, e) -> fprintf oc "%s%a" op print_expr e
  | E_Pair (e1, e2) -> fprintf oc "(%a, %a)" print_expr e1 print_expr e2
  | E_Tyannot (e, te) -> fprintf oc "(%a : %a)" print_expr e print_texpr te

let print_topdef oc = function
  | S_Expr e -> print_expr oc e
  | S_Let (f, e1) ->
      let params, e = un_body [] e1 in
      fprintf oc "let %s %a= %a" f
        (fun oc -> List.iter (fun s -> fprintf oc "%s " s))
        params print_expr e
  | S_Letrec (f, e1) ->
      let params, e = un_body [] e1 in
      fprintf oc "let rec %s %a= %a" f
        (fun oc -> List.iter (fun s -> fprintf oc "%s " s))
        params print_expr e
