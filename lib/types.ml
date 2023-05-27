exception Error of string

type var_name = string

(* Unit�s. *)
type unit_t =
  | UVar of var_name
  | UBase of string
  | UOne
  | UProd of (unit_t * unit_t)
  | UDiv of (unit_t * unit_t)
  | UPow of (unit_t * int)

(* Types. *)
type ty_t =
  | TVar of var_name
  | TBase of (string * unit_t)
  | TFun of (ty_t * ty_t)
  | TPair of (ty_t * ty_t)

(* Génère une nouvelle variable de type. *)
let type_var, unit_var =
  let cpt = ref 0 in
  ( (function
    | () ->
        cpt := !cpt + 1;
        TVar ("t" ^ string_of_int !cpt)),
    function
    | () ->
        cpt := !cpt + 1;
        UVar ("u" ^ string_of_int !cpt) )

let type_int () = TBase ("int", unit_var ())

(*unit a rajouter*)
let type_bool () = TBase ("bool", UOne)
let type_string () = TBase ("string", UOne)
let type_pair t1 t2 = TPair (t1, t2)
let type_fun t1 t2 = TFun (t1, t2)
let type_basic n u = TBase (n, u)

(* Schémas de type. *)
type sch_t = var_name list * ty_t

(* Environnements de typage. *)
type env_t = (string * sch_t) list

(* Environnement initial : contient les constantes true et false. *)
let init_env : (string * (string list * ty_t)) list ref =
  ref
    [
      ("true", ([], TBase ("bool", UOne))); ("false", ([], TBase ("bool", UOne)));
    ]

(* Retourne une instance fraîche de schéma de type. *)
let instance sch =
  let var_mapping = List.map (fun v -> (v, type_var ())) (fst sch) in
  let rec rec_copy ty =
    match ty with
    | TBase _ -> ty
    | TFun (ty1, ty2) -> TFun (rec_copy ty1, rec_copy ty2)
    | TPair (ty1, ty2) -> TPair (rec_copy ty1, rec_copy ty2)
    | TVar v_name -> ( try List.assoc v_name var_mapping with Not_found -> ty)
  in
  rec_copy (snd sch)

(* V�rifie si un nom de variable de type appara�t dans un type. *)
let appear_in_ty v_name ty =
  let rec rec_appear = function
    | TBase _ -> false
    | TFun (ty1, ty2) | TPair (ty1, ty2) -> rec_appear ty1 || rec_appear ty2
    | TVar v_name' -> v_name = v_name'
  in
  rec_appear ty

(* V�rifie si un nom de variable de type appara�t quelque part dans les
   types/sch�mas enregistr�s dans un environnement. *)
let appear_in_env v_name env =
  List.exists (fun (_, sch) -> appear_in_ty v_name (snd sch)) env

(* Retourne un sch�ma de type trivial (pas de g�n�ralisation). *)
let trivial_sch ty = ([], ty)

(* Generalise un type par rapport � un environnement. *)
let generalize ty env =
  let rec find_gen_vars accu = function
    | TBase _ -> accu
    | TFun (ty1, ty2) | TPair (ty1, ty2) ->
        let accu' = find_gen_vars accu ty1 in
        find_gen_vars accu' ty2
    | TVar v_name ->
        if not (appear_in_env v_name env) then v_name :: accu else accu
  in
  (find_gen_vars [] ty, ty)

let rec print_unit ppf = function
  | UVar v -> Printf.fprintf ppf "'%s" v
  | UBase name -> Printf.fprintf ppf "%s" name
  | UOne -> Printf.fprintf ppf "1"
  | UProd (u1, u2) -> Printf.fprintf ppf "(%a.%a)" print_unit u1 print_unit u2
  | UDiv (u1, u2) -> Printf.fprintf ppf "(%a/%a)" print_unit u1 print_unit u2
  | UPow (u, i) -> Printf.fprintf ppf "(%a)^%d" print_unit u i

(* Pretty-printer pour les types. *)
let rec print ppf = function
  | TBase (tname, units) -> Printf.fprintf ppf "%s<%a>" tname print_unit units
  | TFun (t1, t2) -> Printf.fprintf ppf "(%a -> %a)" print t1 print t2
  | TVar v -> Printf.fprintf ppf "'%s" v
  | TPair (t1, t2) -> Printf.fprintf ppf "(%a * %a)" print t1 print t2
