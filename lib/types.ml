exception Error of string ;;

type var_name = string ;;

(* Types. *)
type ty =
  | TInt
  | TBool
  | TString
  | TFun of (ty * ty)
  | TVar of var_name
  | TPair of (ty * ty)
;;


(* Schémas de type. *)
type sch = ((var_name list) * ty) ;;

(* Environnements de typage. *)
type env = (string * sch) list ;;


(* Environnement initial : contient les constantes true et false. *)
let init_env = [
  ("true", ([], TBool)) ;
  ("false", ([], TBool))
] ;;


(* Génère une nouvelle variable de type. *)
let new_ty_var =
  let cpt = ref 0 in
  function () -> cpt := !cpt + 1 ; TVar ("t" ^ (string_of_int !cpt))
;;

(* Retourne une instance fraîche de schéma de type. *)
let instance sch =
  let var_mapping = List.map (fun v -> (v, new_ty_var ())) (fst sch) in
  let rec rec_copy ty =
    match ty with
    | TInt | TBool | TString -> ty
    | TFun (ty1, ty2) -> TFun ((rec_copy ty1), (rec_copy ty2))
    | TPair (ty1, ty2) -> TPair ((rec_copy ty1), (rec_copy ty2))
    | TVar v_name -> (try List.assoc v_name var_mapping with Not_found -> ty) in
  rec_copy (snd sch)
;;


(* Vérifie si un nom de variable de type apparaît dans un type. *)
let appear_in_ty v_name ty =
  let rec rec_appear = function
    | TInt | TBool | TString -> false
    | TFun (ty1, ty2) | TPair (ty1, ty2) -> (rec_appear ty1) || (rec_appear ty2)
    | TVar v_name' -> v_name = v_name' in
  rec_appear ty
;;


(* Vérifie si un nom de variable de type apparaît quelque part dans les
   types/schémas enregistrés dans un environnement. *)
let appear_in_env v_name env =
  List.exists (fun (_, sch) -> appear_in_ty v_name (snd sch)) env ;;


(* Retourne un schéma de type trivial (pas de généralisation). *)
let trivial_sch ty = ([], ty) ;;


(* Generalise un type par rapport à un environnement. *)
let generalize ty env =
  let rec find_gen_vars accu = function
    | TInt | TBool | TString -> accu
    | TFun (ty1, ty2) | TPair (ty1, ty2) ->
        let accu' = find_gen_vars accu ty1 in
        find_gen_vars accu' ty2
    | TVar v_name ->
        if not (appear_in_env v_name env) then v_name :: accu else accu in
  ((find_gen_vars [] ty), ty)
;;


(* Pretty-printer pour les types. *)
let rec print ppf = function
  | TInt -> Printf.fprintf ppf "int"
  | TBool -> Printf.fprintf ppf "bool"
  | TString -> Printf.fprintf ppf "string"
  | TFun (t1, t2) -> Printf.fprintf ppf "(%a -> %a)" print t1 print t2
  | TVar v -> Printf.fprintf ppf "'%s" v
  | TPair (t1, t2) -> Printf.fprintf ppf "(%a * %a)" print t1 print t2
;;
