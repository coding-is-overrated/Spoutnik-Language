exception TypeCycle of (Types.var_name * Types.ty_t)
exception UnitCycle of (Types.var_name * Types.unit_t)
exception TypeConflict of (Types.ty_t * Types.ty_t)

exception UnitConflict of (Types.unit_t * Types.unit_t)
(** Exceptions pour signaler les 4 cas d'erreurs d'unification. *)

(** Fonction qui détecte les cycles dans un problème d'unification pour les types.
   Elle sert à vérifier que l'on n'a pas à unifier quelque chose de la forme v et v -> v
   car dans ce cas, il n'y a pas de solution.
   On recherche donc s'il n'y a pas la variable v dans le type passé en paramètre. *)
let rec occur_check v_name = function
  | Types.TBase _ -> false
  | Types.TVar n -> v_name = n
  | Types.TFun (t1, t2) | Types.TPair (t1, t2) ->
      occur_check v_name t1 || occur_check v_name t2

(** Fonction qui détecte les cycles dans un problème d'unification pour les unités *)
let rec occur_check_unit v_name = function
  | Types.UBase _ | Types.UOne -> false
  | Types.UVar n -> v_name = n
  | Types.UProd (u1, u2) | Types.UDiv (u1, u2) ->
      occur_check_unit v_name u1 || occur_check_unit v_name u2
  | Types.UPow (u, _) -> occur_check_unit v_name u

(** Unification de deux unités. Retourne la substitution à effectuer pour unifier les 2 unités *)
let rec unify_unit u1 u2 =
  if u1 = u2 then []
  else
    match (u1, u2) with
    | Types.UOne, Types.UOne -> []
    | Types.UBase s1, Types.UBase s2 ->
        if s1 = s2 then [] else raise (UnitConflict (u1, u2))
    | Types.UProd (a, b), Types.UProd (c, d)
    | Types.UDiv (a, b), Types.UDiv (c, d) ->
        let s1 = unify_unit a c in
        let b' = Subst.apply_unit b s1 in
        let d' = Subst.apply_unit d s1 in
        Subst.compose_unit (unify_unit b' d') s1
    | Types.UPow (a, i1), Types.UPow (b, i2) ->
        if i1 <> i2 then raise (UnitConflict (u1, u2)) else unify_unit a b
    | Types.UVar v, other | other, Types.UVar v ->
        if occur_check_unit v other then raise (UnitCycle (v, other));
        Subst.singleton v other (*générique*)
    | _ -> raise (UnitConflict (u1, u2))

(** Unification de deux types paramétré. Retourne la substitution à effectuer pour unifier les 2 types.
    La structure d'une substitution est une paire de listes :
    -   types : (var_name, ty_t) list, ou chaque paire signifie T"var_name" devient "ty_t"
    -   units : (var_name, unit_t) list, ou chaque paire signifie U"var_name" devient "unit_t"

    On unifie donc à la fois sur le type (int,float,bool etc.) et sur les unités utilisés *)
let rec unify t1 t2 =
  if t1 = t2 then Subst.empty
  else
    match (t1, t2) with
    | Types.TBase (s1, u1), Types.TBase (s2, u2) ->
        if s1 <> s2 then raise (TypeConflict (t1, t2))
        else { Subst.empty with units = unify_unit u1 u2 }
    | Types.TFun (a, b), Types.TFun (c, d) ->
        let s1 = unify a c in
        (* s1 est la substitution nécessaire pour que a et c collent. *)
        let b' = Subst.apply b s1 in
        let d' = Subst.apply d s1 in
        Subst.compose (unify b' d') s1
    | Types.TPair (a, b), Types.TPair (c, d) ->
        let s1 = unify a c in
        (* s1 est la substitution nécessaire pour que a et c collent. *)
        let b' = Subst.apply b s1 in
        let d' = Subst.apply d s1 in
        Subst.compose (unify b' d') s1
    | Types.TVar v, other | other, Types.TVar v ->
        if occur_check v other then raise (TypeCycle (v, other));
        { Subst.empty with types = Subst.singleton v other }
    | _ -> raise (TypeConflict (t1, t2))
