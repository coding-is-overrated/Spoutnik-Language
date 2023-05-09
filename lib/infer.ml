(* Recherche du type d'un identificateur dans un environment. *)
let rec find_id_sch idname = function
  | [] -> raise (Types.Error ("unbound identifier " ^ idname))
  | h :: q -> if fst h = idname then snd h else find_id_sch idname q
;;


let rec convert_uexpr_into_unitt e = match e with
  |Pcfast.UE_U0 -> Types.UOne
  |Pcfast.UE_Base s -> Types.UBase (s)
  |Pcfast.UE_Mul (u1, u2) -> Types.UProd(convert_uexpr_into_unitt u1,convert_uexpr_into_unitt u2)
  |Pcfast.UE_Div (u1, u2) -> Types.UDiv(convert_uexpr_into_unitt u1,convert_uexpr_into_unitt u2)
  |Pcfast.UE_Pow (u, i) -> Types.UPow(convert_uexpr_into_unitt u,i)

(* Conversion d'une expression de type en type. Doit maintenir la
   correspondance entre les noms de variables d�j� rencontr�s et les variables
   de type cr��es pour les rept�senter. *)
let type_of_type_expr te =
  let var_mapping = ref [] in
  let rec rec_convert = function
    | Pcfast.TE_Var vn -> (
        try List.assoc vn !var_mapping with Not_found ->
          let new_v = Types.type_var () in
          var_mapping := (vn, new_v) :: !var_mapping ;
          new_v
        )
    | Pcfast.TE_Basic (n,u_) ->
        let u = convert_uexpr_into_unitt u_ in
        Types.type_basic n u
    | Pcfast.TE_Fun (te1, te2) ->
        let t1 = rec_convert te1 in
        let t2 = rec_convert te2 in
        Types.type_fun t1 t2
    | Pcfast.TE_Pair (te1, te2) ->
        let t1 = rec_convert te1 in
        let t2 = rec_convert te2 in
        Types.type_pair t1 t2 in
  rec_convert te
;;


(* Inf�rence du type d'une expression. *)
let rec type_expr env = function
  | Pcfast.E_Int _ -> (Types.type_int (), Subst.empty) (*changer ici*)
  | Pcfast.E_Bool _ -> (Types.type_bool (), Subst.empty)
  | Pcfast.E_String _ -> (Types.type_string (), Subst.empty)
  | Pcfast.E_Ident v -> (Types.instance (find_id_sch v env), Subst.empty)
  | Pcfast.E_If (cond_e, then_e, else_e) -> (
      (* Typage de la condition. *)
      let (cond_ty, cond_subst) = type_expr env cond_e in
      (* Forcer le type � �tre bool. *)
      let u = Unify.unify cond_ty (Types.type_bool ()) in
      (* Typage des 2 branches du if. *)
      let (then_ty, st) =
        type_expr (Subst.subst_env (Subst.compose u cond_subst) env) then_e in
      let (else_ty, se) =
        type_expr
          (Subst.subst_env (Subst.compose st (Subst.compose u cond_subst)) env)
          else_e in
      (* Unification des 2 branches de la conditionnelle. *)
      let u' = Unify.unify (Subst.apply then_ty se) else_ty in
      ((Subst.apply else_ty u'),
       (Subst.compose
          u'
          (Subst.compose se (Subst.compose st (Subst.compose u cond_subst)))))
     )
  | Pcfast.E_Fun (arg_name, body) ->
      (* Nouvelle variable de type pour le param�tre de la fonction. *)
      let arg_ty = Types.type_var () in
      (* Type et substitution du corps. *)
      let (body_ty, body_sub) =
        type_expr ((arg_name, (Types.trivial_sch arg_ty)) :: env) body in
      (Types.TFun ((Subst.apply arg_ty body_sub, body_ty)), body_sub)
  | Pcfast.E_Let (v_name, e1, e2) ->
      let (e1_ty, e1_sub) = type_expr env e1 in
      let new_env = Subst.subst_env e1_sub env in
      let new_env' = (v_name, (Types.generalize e1_ty new_env)) :: new_env in
      let (e2_ty, e2_sub) = type_expr new_env' e2 in
      (e2_ty, Subst.compose e2_sub e1_sub)
  | Pcfast.E_Letrec (v_name, e1, e2) ->
      (* Nouvelle variable qui deviendra le type de la fonction apr�s l'avoir
         unifi� avec le type calcul� par l'inf�rence pour le corps de la
         d�finition. *)
      let e1_pre_ty = Types.type_var () in
      (* Nouvel environnement pour typer le corps de la d�finition. On ne
         g�n�ralise pas le type temporairement donn� pour la d�finition. *)
      let new_env = (v_name, (Types.trivial_sch e1_pre_ty)) :: env in
      let (e1_ty, e1_sub) = type_expr new_env e1 in
      (* Il faut unifier le type suppos� et le type trouv�. *)
      let u = Unify.unify e1_pre_ty e1_ty in
      let e1_ty' = Subst.apply e1_ty u in
      (* Maintenant il faut typer la partie 'in' dans l'environnement �tendu
         par le type finalement d�termin� pour l'identificateur li�
         r�cursivement. *)
      let new_env2 = Subst.subst_env (Subst.compose u e1_sub) env in
      let new_env3 = (v_name, (Types.generalize e1_ty' new_env2)) :: new_env2 in
      let (e2_ty, e2_sub) = type_expr new_env3 e2 in
      (e2_ty, Subst.compose e2_sub (Subst.compose u e1_sub))
  | Pcfast.E_App (e1, e2) ->
      let (e1_ty, e1_sub) = type_expr env e1 in
      let (e2_ty, e2_sub) = type_expr (Subst.subst_env e1_sub env) e2 in
      (* Nouvelle variable de type. On sait que e1_ty doit �tre de la forme
         t1 -> t2. Notre nouvelle variable va faire office de t2. *)
      let tmp_ty = Types.type_var () in
      let e1_ty' = (Subst.apply e1_ty e2_sub) in
      let u = Unify.unify (Types.TFun (e2_ty, tmp_ty)) e1_ty' in
      (Subst.apply tmp_ty u,
       (Subst.compose u (Subst.compose e2_sub e1_sub)))
  (*
  | Pcfast.Pair (e1, e2) ->
      let (e1_ty, e1_sub) = type_expr env e1 in
      let (e2_ty, e2_sub) = type_expr (Subst.subst_env e1_sub env) e2 in
      let e1_ty' = Subst.apply e1_ty e2_sub in
      (Types.T_pair (e1_ty', e2_ty), Subst.compose e2_sub e1_sub)
   *)
  | Pcfast.E_Binop (o_name, e1, e2) -> (
      match o_name with
      | "+" | "-" | "/" | "*" ->
          (* Typage des 2 op�randes. *)
          let (e1_ty, e1_sub) = type_expr env e1 in
          let (e2_ty, e2_sub) = type_expr (Subst.subst_env e1_sub env) e2 in
          (* On les force � avoir le m�me type. *)
          let u = Unify.unify e1_ty e2_ty in
          let e1_ty' = Subst.apply e1_ty u in
          (* On force ce m�me type � �tre int. On utilise le type de l'une
             des op�randes au pif. *)
          let u' = Unify.unify e1_ty' (Types.type_int ()) in
          (* On retourne int. *)
          ((Types.type_int ()),
           (Subst.compose u' (Subst.compose u (Subst.compose e2_sub e1_sub))))
      | "=" | ">" | ">=" | "<" | "<=" ->
          (* Op�rateurs polymorphes. La seule contrainte est que les deux
             op�randes aient le m�me type. *)
          let (e1_ty, e1_sub) = type_expr env e1 in
          let (e2_ty, e2_sub) = type_expr (Subst.subst_env e1_sub env) e2 in
          let u = Unify.unify e1_ty e2_ty in
          (* Retourner bool. *)
          ((Types.type_bool ()),
           (Subst.compose u (Subst.compose e2_sub e1_sub)))
      | _ -> failwith "Unknown binop"
     )
  | Pcfast.E_Monop (o_name, e) -> (
      match o_name with
      | "-" ->
          (* Typage de l'op�rande. *)
          let (e1_ty, e1_sub) = type_expr env e in
          (* On force ce type � �tre int. *)
          let u = Unify.unify e1_ty (Types.type_int ()) in
          (* On retourne int. *)
          ((Types.type_int ()), (Subst.compose u e1_sub))
      | _ -> failwith "Unknown monop"
     )
  | Pcfast.E_Pair (e1, e2) ->
      let (e1_ty, e1_sub) = type_expr env e1 in
      let (e2_ty, e2_sub) = type_expr (Subst.subst_env e1_sub env) e2 in
      ((Types.type_pair e1_ty e2_ty),
       (Subst.compose e2_sub e1_sub))
  | Pcfast.E_Tyannot (e, te) ->
      let t_of_te = type_of_type_expr te in
      let (e_ty, e_sub) = type_expr env e in
      let u = Unify.unify t_of_te e_ty in
      let e_ty' = Subst.apply e_ty u in
      (e_ty', (Subst.compose u e_sub))
;;


(* Inf�rence du type d'une phrase toplevel. Retourne le type (corps du sch�ma)
   et le nouvel environnement dans lequel l'�ventuelle nouvelle liaison a �t�
   rajout� � l'environnement initialement re�u. *)
let type_topdef env = function
  | Pcfast.S_Expr e -> (fst (type_expr env e), env)
  | Pcfast.S_Let (v_name, e1) ->
      (* Similaire au let des expressions. *)
      let (e1_ty, e1_sub) = type_expr env e1 in
      let new_env = Subst.subst_env e1_sub env in
      let new_env' = (v_name, (Types.generalize e1_ty new_env)) :: new_env in
      (e1_ty, new_env')
  | Pcfast.S_Letrec (v_name, e1) ->
      (* Similaire au let rec des expressions. *)
      let e1_pre_ty = Types.type_var () in
      let new_env = (v_name, (Types.trivial_sch e1_pre_ty)) :: env in
      let (e1_ty, e1_sub) = type_expr new_env e1 in
      let u = Unify.unify e1_pre_ty e1_ty in
      let e1_ty' = Subst.apply e1_ty u in
      let new_env2 = Subst.subst_env (Subst.compose u e1_sub) env in
      let new_env3 = (v_name, (Types.generalize e1_ty' new_env2)) :: new_env2 in
      (e1_ty', new_env3)
;;

