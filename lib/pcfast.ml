(* Ce fichier contient la définition du type OCaml des arbres de
 * syntaxe abstraite du langage, ainsi qu'un imprimeur des phrases
 * du langage.
 *)

type expr =
  | Int of int
  | Bool of bool
  | String of string
  | Ident of string
  | App of expr * expr
  | Monop of string * expr
  | Binop of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | Let of string * expr * expr
  | Letrec of string * expr * expr
;;

open Printf;;

let rec un_body params = function
  | Fun(p, e) -> un_body (p :: params) e
  | e -> (params, e)
;;

let rec print oc e =
  match e with
  | Int n -> fprintf oc "%d" n
  | Bool b -> fprintf oc "%s" (if b then "T" else "F")
  | Ident s -> fprintf oc "%s" s
  | String s -> fprintf oc "\"%s\"" s
  | App (e1, e2) -> fprintf oc "(%a %a)" print e1 print e2
  | Let (f, e1, e2) ->
      let (params, e) = un_body [] e1 in
      fprintf oc "(let %s %a= %a in %a)"
        f
        (fun oc -> List.iter (fun s -> fprintf oc "%s " s)) params
        print e
        print e2
  | Letrec (f, e1, e2) ->
      let (params, e) = un_body [] e1 in
      fprintf oc "(letrec %s %a= %a in %a)"
        f
        (fun oc -> List.iter (fun s -> fprintf oc "%s " s)) params
        print e
        print e2
  | Fun (x, e) -> fprintf oc "(fun %s -> %a)"  x print e
  | If (test, e1, e2) ->
      fprintf oc "(if %a then %a else %a)" print test print e1 print e2
  | Binop (op,e1,e2) -> fprintf oc "(%a %s %a)" print e1 op print e2
  | Monop (op,e) -> fprintf oc "%s%a" op print e
;;
