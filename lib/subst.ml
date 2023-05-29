(* Type des substitutions. *)
type subst = {
  types : (Types.var_name * Types.ty_t) list;
  units : (Types.var_name * Types.unit_t) list;
}
(** La structure d'une substitution est une paire de listes :
   -   types : (var_name, ty_t) list, ou chaque paire signifie T"var_name" devient "ty_t"
   -   units : (var_name, unit_t) list, ou chaque paire signifie U"var_name" devient "unit_t" *)

(** Substitution vide, donc n'ayant aucune substitution de types et d'units*)
let empty = { types = []; units = [] }

let singleton var_name ty = [ (var_name, ty) ]

(** Applique une seule transformation à un type : c'est l'étape élémentaire d'une substitution. *)
let rec apply_one_type transf = function
  | Types.TFun (t1, t2) ->
      Types.TFun (apply_one_type transf t1, apply_one_type transf t2)
  | Types.TVar var_name ->
      if fst transf = var_name then snd transf else Types.TVar var_name
  | Types.TPair (t1, t2) ->
      Types.TPair (apply_one_type transf t1, apply_one_type transf t2)
  | any -> any (* cas de TBase, pas de subst à faire*)

(** Applique une seule transformation à un unit : c'est l'étape élémentaire d'une substitution*)
let rec apply_one_unit transf = function
  | Types.UVar var_name ->
      if fst transf = var_name then snd transf else Types.UVar var_name
  | Types.UProd (u1, u2) ->
      Types.UProd (apply_one_unit transf u1, apply_one_unit transf u2)
  | Types.UDiv (u1, u2) ->
      Types.UDiv (apply_one_unit transf u1, apply_one_unit transf u2)
  | Types.UPow (u, i) -> Types.UPow (apply_one_unit transf u, i)
  | any -> any (* cas de Ubase et UOne, pas de subst à faire*)

(** Applique une substitution à un type. *)
let rec apply_type ty = function
  | [] -> ty
  | h :: q -> apply_type (apply_one_type h ty) q

(** Applique une substitution à un unit. *)
let rec apply_unit u = function
  | [] -> u
  | h :: q -> apply_unit (apply_one_unit h u) q

(** Applique une substitution complète (types et unités) *)
let rec apply ty subst =
  match ty with
  | Types.TBase (s, u) -> Types.TBase (s, apply_unit u subst.units)
  | Types.TFun (t1, t2) -> Types.TFun (apply t1 subst, apply t2 subst)
  | Types.TPair (t1, t2) -> Types.TPair (apply t1 subst, apply t2 subst)
  | Types.TVar n -> (
      match List.assoc_opt n subst.types with Some ty -> ty | None -> ty)

(** Applique une substitution à tous les types d'un environnement de typage
   et retourne le nouvel environnement. *)
let subst_env subst env : Types.env_t =
  List.map
    (fun (name, (tvars, uvars, ty)) -> (name, (tvars, uvars, apply ty subst)))
    env

(** Retourne la substitution subst1 restreinte à tout ce qui n'est pas dans
   le domaine de subst2. *)
let restrict subst1 subst2 =
  (*rec*)
  List.filter (fun (name, _) -> not (List.mem_assoc name subst2)) subst1

(** Composition de 2 substitutions POUR LES TYPES.
   Retourne une nouvelle substitution theta2 rond theta1. *)
let compose_type theta2 theta1 =
  let theta = List.map (fun (v, t) -> (v, apply_type t theta2)) theta1 in
  (* On retire de subst2 ce qui appartient au domaine de subst1 puisque �a
     ne sert plus, subst1 ayant �t� apliqu�e en premier, ses modifications
     sont "prioritaires". *)
  let subst2_minus_subst1 = restrict theta2 theta1 in
  theta @ subst2_minus_subst1

(** Composition de 2 substitutions POUR LES UNIT.
   Retourne une nouvelle substitution theta2 rond theta1. *)
let compose_unit theta2 theta1 =
  let theta = List.map (fun (v, t) -> (v, apply_unit t theta2)) theta1 in
  (* On retire de subst2 ce qui appartient au domaine de subst1 puisque �a
     ne sert plus, subst1 ayant �t� apliqu�e en premier, ses modifications
     sont "prioritaires". *)
  let subst2_minus_subst1 = restrict theta2 theta1 in
  theta @ subst2_minus_subst1

(** Composition de 2 substitutions complète (types et units).
   Retourne une nouvelle substitution theta2 rond theta1. *)
let compose theta2 theta1 =
  let compu = compose_unit theta2.units theta1.units in
  let compt = compose_type theta2.types theta1.types in
  {
    types =
      List.map (fun (v, t) -> (v, apply t { types = []; units = compu })) compt;
    (* Parce que les types contiennent des unités *)
    (* Peut-être pas optimal mais permet d'assurer une inférence correcte *)
    units = compu;
  }
