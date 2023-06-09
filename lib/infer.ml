(** Recherche du type d'un identificateur dans un environment. *)
let rec find_id_sch idname = function
  | [] -> raise (Types.Error ("unbound identifier " ^ idname))
  | h :: q -> if fst h = idname then snd h else find_id_sch idname q

(** Conversion d'une expression d'unité en unité. *)
let rec convert_uexpr_into_unitt e =
  match e with
  | Ast.UE_UOne -> Types.UOne
  | Ast.UE_Base s -> Types.UBase s
  | Ast.UE_Mul (u1, u2) ->
      Types.UProd (convert_uexpr_into_unitt u1, convert_uexpr_into_unitt u2)
  | Ast.UE_Div (u1, u2) ->
      Types.UDiv (convert_uexpr_into_unitt u1, convert_uexpr_into_unitt u2)
  | Ast.UE_Pow (u, i) -> Types.UPow (convert_uexpr_into_unitt u, i)

(** Conversion d'une expression de type en type. Doit maintenir la
    correspondance entre les noms de variables déjà rencontrés et les variables
    de type créées pour les représenter. *)
let type_of_type_expr te =
  let var_mapping = ref [] in
  let rec rec_convert = function
    | Ast.TE_Var vn -> (
        try List.assoc vn !var_mapping
        with Not_found ->
          let new_v = Types.type_var () in
          var_mapping := (vn, new_v) :: !var_mapping;
          new_v)
    | Ast.TE_Basic (n, u_) ->
        let u = convert_uexpr_into_unitt u_ in
        Types.type_basic n (Simplify.simplify u)
    | Ast.TE_Fun (te1, te2) ->
        let t1 = rec_convert te1 in
        let t2 = rec_convert te2 in
        Types.type_fun t1 t2
    | Ast.TE_Pair (te1, te2) ->
        let t1 = rec_convert te1 in
        let t2 = rec_convert te2 in
        Types.type_pair t1 t2
  in
  rec_convert te

(** Inférence du type d'une expression. *)
let rec type_expr env = function
  | Ast.E_Int _ -> (Types.type_int (), Subst.empty)
  | Ast.E_Float _ -> (Types.type_float (), Subst.empty)
  | Ast.E_Bool _ -> (Types.type_bool (), Subst.empty)
  | Ast.E_String _ -> (Types.type_string (), Subst.empty)
  | Ast.E_Ident v -> (Types.instance (find_id_sch v env), Subst.empty)
  | Ast.E_If (cond_e, then_e, else_e) ->
      (* Typage de la condition. *)
      let cond_ty, cond_subst = type_expr env cond_e in
      (* Forcer le type à être bool. *)
      let u = Unify.unify cond_ty (Types.type_bool ()) in
      (* Typage des 2 branches du if. *)
      let then_ty, st =
        type_expr (Subst.subst_env (Subst.compose u cond_subst) env) then_e
      in
      let else_ty, se =
        type_expr
          (Subst.subst_env (Subst.compose st (Subst.compose u cond_subst)) env)
          else_e
      in
      (* Unification des 2 branches de la conditionnelle. *)
      let u' = Unify.unify (Subst.apply then_ty se) else_ty in
      ( Subst.apply else_ty u',
        Subst.compose u'
          (Subst.compose se (Subst.compose st (Subst.compose u cond_subst))) )
  | Ast.E_Fun (arg_name, body) ->
      (* Nouvelle variable de type pour le paramètre de la fonction. *)
      let arg_ty = Types.type_var () in
      (* Type et substitution du corps. *)
      let body_ty, body_sub =
        type_expr ((arg_name, Types.trivial_sch arg_ty) :: env) body
      in
      (Types.TFun (Subst.apply arg_ty body_sub, body_ty), body_sub)
  | Ast.E_Let (v_name, e1, e2) ->
      let e1_ty, e1_sub = type_expr env e1 in
      let new_env = Subst.subst_env e1_sub env in
      let new_env' = (v_name, Types.generalize e1_ty new_env) :: new_env in
      let e2_ty, e2_sub = type_expr new_env' e2 in
      (e2_ty, Subst.compose e2_sub e1_sub)
  | Ast.E_Letrec (v_name, e1, e2) ->
      (* Nouvelle variable qui deviendra le type de la fonction après l'avoir
         unifié avec le type calculé par l'inférence pour le corps de la
         définition. *)
      let e1_pre_ty = Types.type_var () in
      (* Nouvel environnement pour typer le corps de la définition. On ne
         généralise pas le type temporairement donné pour la définition. *)
      let new_env = (v_name, Types.trivial_sch e1_pre_ty) :: env in
      let e1_ty, e1_sub = type_expr new_env e1 in
      (* Il faut unifier le type supposé et le type trouvé. *)
      let u = Unify.unify e1_pre_ty e1_ty in
      let e1_ty' = Subst.apply e1_ty u in
      (* Maintenant il faut typer la partie 'in' dans l'environnement étendu
         par le type finalement déterminé pour l'identificateur lié
         récursivement. *)
      let new_env2 = Subst.subst_env (Subst.compose u e1_sub) env in
      let new_env3 = (v_name, Types.generalize e1_ty' new_env2) :: new_env2 in
      let e2_ty, e2_sub = type_expr new_env3 e2 in
      (e2_ty, Subst.compose e2_sub (Subst.compose u e1_sub))
  | Ast.E_App (func, arg) ->
      let func_t, func_s = type_expr env func in
      let arg_t, arg_s = type_expr (Subst.subst_env func_s env) arg in
      (* Nouvelle variable de type. On sait que e1_ty doit être de la forme
         t1 -> t2. Notre nouvelle variable va faire office de t2. *)
      let ret_t = Types.type_var () in
      let func_t' = Subst.apply func_t arg_s in
      let mu = Unify.unify (Types.TFun (arg_t, ret_t)) func_t' in
      (Subst.apply ret_t mu, Subst.compose mu (Subst.compose arg_s func_s))
  | Ast.E_Binop (o_name, e1, e2) -> (
      match o_name with
      | "+" | "-" ->
          (* Typage des 2 opérandes. *)
          let e1_ty, e1_sub = type_expr env e1 in
          let e2_ty, e2_sub = type_expr (Subst.subst_env e1_sub env) e2 in
          (* On les force à avoir le même type. *)
          let e1_ty = Subst.apply e1_ty e2_sub in
          let u = Unify.unify e1_ty e2_ty in
          let e1_ty' = Subst.apply e1_ty u in
          (* On force ce même type à être int. On utilise le type de l'une des opérandes au pif. *)
          let u' =
            match e1_ty' with
            | TBase (("int" | "float"), _) -> Subst.empty
            | _ -> Unify.unify e1_ty' (Types.type_float ())
            (*par défaut float*)
          in
          (* On retourne int. *)
          ( Subst.apply e1_ty' u',
            Subst.compose u' (Subst.compose u (Subst.compose e2_sub e1_sub)) )
      | "/" | "*" ->
          (* Typage des 2 opérandes. *)
          let e1_ty, e1_sub = type_expr env e1 in
          let e2_ty, e2_sub = type_expr (Subst.subst_env e1_sub env) e2 in
          (* On unifie les unités *)
          let e1_ty = Subst.apply e1_ty e2_sub in
          let u1 =
            match e1_ty with
            | TBase (("int" | "float"), _) -> Subst.empty
            | _ -> Unify.unify e1_ty (Types.type_float ())
            (*par défaut float*)
          in
          let e2_ty = Subst.apply e2_ty u1 in
          let u2 =
            match e2_ty with
            | TBase (("int" | "float"), _) -> Subst.empty
            | _ -> Unify.unify e2_ty (Types.type_float ())
            (*par défaut float*)
          in
          let e1_ty' = Subst.apply e1_ty u1 in
          let e2_ty' = Subst.apply e2_ty u2 in
          let t =
            match (e1_ty', e2_ty') with
            | Types.TBase ("int", un1), Types.TBase ("int", un2) ->
                if o_name = "*" then
                  (*multiplication et ses règles*)
                  Types.TBase ("int", Simplify.simplify (Types.UProd (un1, un2)))
                else
                  (* division et ses règles *)
                  Types.TBase ("int", Simplify.simplify (Types.UDiv (un1, un2)))
            | Types.TBase ("float", un1), Types.TBase ("float", un2) ->
                if o_name = "*" then
                  (*multiplication et ses règles*)
                  Types.TBase
                    ("float", Simplify.simplify (Types.UProd (un1, un2)))
                else
                  (* division et ses règles *)
                  Types.TBase
                    ("float", Simplify.simplify (Types.UDiv (un1, un2)))
            | _ ->
                failwith
                  "Multiplication or division applied to something other than \
                   int or float or tried to multiply an int with a float"
          in
          (* On retourne int produit *)
          (t, Subst.compose u2 (Subst.compose u1 (Subst.compose e2_sub e1_sub)))
      | "=" | ">" | ">=" | "<" | "<=" ->
          (* Opérateurs polymorphes. La seule contrainte est que les deux opérandes aient le même type. *)
          let e1_ty, e1_sub = type_expr env e1 in
          let e2_ty, e2_sub = type_expr (Subst.subst_env e1_sub env) e2 in
          let u = Unify.unify e1_ty e2_ty in
          (* Retourner bool. *)
          (Types.type_bool (), Subst.compose u (Subst.compose e2_sub e1_sub))
      | _ -> failwith "Unknown binop")
  | Ast.E_Monop (o_name, e) -> (
      match o_name with
      | "-" ->
          (* Typage de l'op�rande. *)
          let e1_ty, e1_sub = type_expr env e in
          (* On force ce type � �tre int. *)
          let u =
            match e1_ty with
            | TBase (("int" | "float"), _) -> Subst.empty
            | _ -> Unify.unify e1_ty (Types.type_float ())
            (*par défaut float*)
          in
          (Subst.apply e1_ty u, Subst.compose u e1_sub)
      | _ -> failwith "Unknown monop")
  | Ast.E_Pair (e1, e2) ->
      let e1_ty, e1_sub = type_expr env e1 in
      let e2_ty, e2_sub = type_expr (Subst.subst_env e1_sub env) e2 in
      (Types.type_pair e1_ty e2_ty, Subst.compose e2_sub e1_sub)
  | Ast.E_Tyannot (e, te) ->
      let t_of_te = type_of_type_expr te in
      let e_ty, e_sub = type_expr env e in
      let u = Unify.unify t_of_te e_ty in
      let e_ty' = Subst.apply e_ty u in
      (e_ty', Subst.compose u e_sub)

(** Inférence du type d'une phrase toplevel. Retourne le type (corps du schéma)
    et le nouvel environnement dans lequel l'éventuelle nouvelle liaison a été
    rajouté à l'environnement initialement reçu. *)
let type_topdef env = function
  | Ast.S_Expr e -> (fst (type_expr env e), env)
  | Ast.S_Let (v_name, e1) ->
      (* Similaire au let des expressions. *)
      let e1_ty, e1_sub = type_expr env e1 in
      let new_env = Subst.subst_env e1_sub env in
      let new_env' = (v_name, Types.generalize e1_ty new_env) :: new_env in
      (e1_ty, new_env')
  | Ast.S_Letrec (v_name, e1) ->
      (* Similaire au let rec des expressions. *)
      let e1_pre_ty = Types.type_var () in
      let new_env = (v_name, Types.trivial_sch e1_pre_ty) :: env in
      let e1_ty, e1_sub = type_expr new_env e1 in
      let u = Unify.unify e1_pre_ty e1_ty in
      let e1_ty' = Subst.apply e1_ty u in
      let new_env2 = Subst.subst_env (Subst.compose u e1_sub) env in
      let new_env3 = (v_name, Types.generalize e1_ty' new_env2) :: new_env2 in
      (e1_ty', new_env3)
