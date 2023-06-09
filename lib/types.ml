exception Error of string

type var_name = string

(** Unités. *)
type unit_t =
  | UVar of var_name
  | UBase of string
  | UOne
  | UProd of (unit_t * unit_t)
  | UDiv of (unit_t * unit_t)
  | UPow of (unit_t * int)

(** Types. *)
type ty_t =
  | TVar of var_name
  | TBase of (string * unit_t)
  | TFun of (ty_t * ty_t)
  | TPair of (ty_t * ty_t)

(** Génère une nouvelle variable de type avec ses unités *)
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

let get_uvar_string = function
  | UVar x -> x
  | _ -> failwith "Tried to get the argument of a UVar but got something else"

let get_tvar_string = function
  | TVar x -> x
  | _ -> failwith "Tried to get the argument of a UVar but got something else"

let type_int () = TBase ("int", unit_var ())
let type_float () = TBase ("float", unit_var ())

(* Par défaut booléens et string sans unités *)
let type_bool () = TBase ("bool", UOne)
let type_string () = TBase ("string", UOne)
let type_pair t1 t2 = TPair (t1, t2)
let type_fun t1 t2 = TFun (t1, t2)
let type_basic n u = TBase (n, u)

type sch_t = var_name list * var_name list * ty_t
(** Schémas de type. 
    La première liste contient les variables de type,
    la seconde les variables d'unité *)

type env_t = (string * sch_t) list
(** Environnements de typage. *)

(** Environnement initial : contient les constantes true et false. *)
let init_env : (string * (string list * string list * ty_t)) list ref =
  ref
    [
      ("true", ([], [], TBase ("bool", UOne)));
      ("false", ([], [], TBase ("bool", UOne)));
      ( "sqrt",
        let units = unit_var () in
        ( [],
          [ get_uvar_string units ],
          TFun (TBase ("float", UPow (units, 2)), TBase ("float", units)) ) );
      ("ln", ([], [], TFun (TBase ("float", UOne), TBase ("float", UOne))));
      ("exp", ([], [], TFun (TBase ("float", UOne), TBase ("float", UOne))));
      ("sin", ([], [], TFun (TBase ("float", UOne), TBase ("float", UOne))));
      ("cos", ([], [], TFun (TBase ("float", UOne), TBase ("float", UOne))));
      ("tan", ([], [], TFun (TBase ("float", UOne), TBase ("float", UOne))));
      ( "print",
        let types = type_var () in
        ([ get_tvar_string types ], [], TFun (types, TBase ("int", UOne))) );
    ]

(* Retourne une instance fraîche de schéma de type. *)
let instance (tvars, uvars, ty) =
  let tvar_mapping = List.map (fun v -> (v, type_var ())) tvars in
  let uvar_mapping = List.map (fun v -> (v, unit_var ())) uvars in
  let rec copy_unit u =
    match u with
    | UOne | UBase _ -> u
    | UPow (u, i) -> UPow (copy_unit u, i)
    | UProd (u1, u2) -> UProd (copy_unit u1, copy_unit u2)
    | UDiv (u1, u2) -> UDiv (copy_unit u1, copy_unit u2)
    | UVar v_name -> ( try List.assoc v_name uvar_mapping with Not_found -> u)
  in
  let rec rec_copy ty =
    match ty with
    | TBase (n, u) -> TBase (n, copy_unit u)
    | TFun (ty1, ty2) -> TFun (rec_copy ty1, rec_copy ty2)
    | TPair (ty1, ty2) -> TPair (rec_copy ty1, rec_copy ty2)
    | TVar v_name -> (
        try List.assoc v_name tvar_mapping with Not_found -> ty)
  in
  rec_copy ty

(** Vérifie si un nom de variable de type apparaît dans un type. *)
let appear_in_ty v_name ty =
  let rec rec_appear = function
    | TBase _ -> false
    | TFun (ty1, ty2) | TPair (ty1, ty2) -> rec_appear ty1 || rec_appear ty2
    | TVar v_name' -> v_name = v_name'
  in
  rec_appear ty

let appear_in_unit v_name ty =
  let rec rec_appear_unit = function
    | UVar v_name' -> v_name = v_name'
    | UBase _ | UOne -> false
    | UProd (u1, u2) | UDiv (u1, u2) -> rec_appear_unit u1 || rec_appear_unit u2
    | UPow (u, _) -> rec_appear_unit u
  in
  let rec rec_appear = function
    | TBase (_, u) -> rec_appear_unit u
    | TFun (ty1, ty2) | TPair (ty1, ty2) -> rec_appear ty1 || rec_appear ty2
    | TVar _ -> false
  in
  rec_appear ty

(** Vérifie si un nom de variable de type apparaît quelque part dans les
   types/schémas enregistrés dans un environnement. *)
let appear_in_env v_name (env : env_t) =
  List.exists (fun (_, (_, _, ty)) -> appear_in_ty v_name ty) env

let appear_in_env_unit v_name (env : env_t) =
  List.exists (fun (_, (_, _, ty)) -> appear_in_unit v_name ty) env

(** Retourne un schéma de type trivial (pas de généralisation). *)
let trivial_sch ty = ([], [], ty)

(** Généralise un type par rapport à un environnement. *)
let generalize ty (env : env_t) : sch_t =
  let rec find_unit_vars accu = function
    | UVar v_name ->
        if not (appear_in_env_unit v_name env) then v_name :: accu else accu
    | UProd (u1, u2) | UDiv (u1, u2) ->
        find_unit_vars (find_unit_vars accu u1) u2
    | UPow (u, _) -> find_unit_vars accu u
    | UBase _ | UOne -> accu
  in
  let rec find_gen_vars (tvars, uvars) = function
    | TBase (_, u) -> (tvars, find_unit_vars uvars u)
    | TFun (ty1, ty2) | TPair (ty1, ty2) ->
        let accu' = find_gen_vars (tvars, uvars) ty1 in
        find_gen_vars accu' ty2
    | TVar v_name ->
        if not (appear_in_env v_name env) then (v_name :: tvars, uvars)
        else (tvars, uvars)
  in
  let tvars, uvars = find_gen_vars ([], []) ty in
  (tvars, uvars, ty)

(** Pretty-printer pour les unités. *)
let rec print_unit ppf = function
  | UVar v -> Printf.fprintf ppf "'%s" v
  | UBase name -> Printf.fprintf ppf "%s" name
  | UOne -> Printf.fprintf ppf "1"
  | UProd (u1, u2) -> Printf.fprintf ppf "(%a.%a)" print_unit u1 print_unit u2
  | UDiv (u1, u2) -> Printf.fprintf ppf "(%a/%a)" print_unit u1 print_unit u2
  | UPow (u, i) -> Printf.fprintf ppf "(%a)^%d" print_unit u i

(** Pretty-printer pour les types. *)
let rec print ppf = function
  | TVar v -> Printf.fprintf ppf "'%s" v
  | TBase (tname, units) -> Printf.fprintf ppf "%s<%a>" tname print_unit units
  | TFun (t1, t2) -> Printf.fprintf ppf "(%a -> %a)" print t1 print t2
  | TPair (t1, t2) -> Printf.fprintf ppf "(%a * %a)" print t1 print t2
