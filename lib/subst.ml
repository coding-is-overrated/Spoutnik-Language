(* Type des substitutions. *)
type subst =  (Types.var_name * Types.ty) list ;;


(* Substitution vide. *)
let empty = [] ;;


let singleton var_name ty = [(var_name, ty)] ;;


(* Applique une seule transformation � un type : c'est l'�tape �l�mentaire
   d'une substitution. *)
let rec apply_one transf = function
  | Types.TFun (t1, t2) ->
      Types.TFun (apply_one transf t1, apply_one transf t2)
  | Types.TVar var_name ->
      if (fst transf) = var_name then snd transf else Types.TVar var_name
  | Types.TPair (t1, t2) ->
      Types.TPair (apply_one transf t1, apply_one transf t2)
  | any -> any
;;


(* Applique une substitution � un type. *)
let rec apply ty = function
  | [] -> ty
  | h :: q -> apply (apply_one h ty) q
;;


(* Applique une substitution � tous les types d'un environnement de typage
   et retourne le nouvel environnement. *)
let subst_env subst env =
  List.map
    (fun (name, (gen_vars, ty)) -> (name, (gen_vars, apply ty subst))) env
;;


(* Retourne la substitution subst1 restreinte � tout ce qui n'est pas dans
   le domaine de subst2. *)
let restrict subst1 subst2 =
  List.filter (fun (name, _) -> not (List.mem_assoc name subst2)) subst1
;;


(* Composition de 2 substitutions.
   Retourne une nouvelle substitution theta2 rond theta1. *)
let compose theta2 theta1 =
  let theta = List.map (fun (v, t) -> (v, apply t theta2)) theta1 in
  (* On retire de subst2 ce qui appartient au domaine de subst1 puisque �a
     ne sert plus, subst1 ayant �t� apliqu�e en premier, ses modifications
     sont "prioritaires". *)
  let subst2_minus_subst1 = restrict theta2 theta1 in
  theta @ subst2_minus_subst1
;;
