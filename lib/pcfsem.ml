open Pcfast

(** Définition des types *)
type pcfval =
  | Intval of int
  | Floatval of float
  | Boolval of bool
  | Stringval of string
  | Funval of (pcfval -> pcfval)
  (*{ param : string; body : expr; env : environment }*)
  (* pcfval -> pcfval *)
  | Funrecval of {
      fname : string;
      param : string;
      body : expr;
      env : environment;
    }
  | Pairval of pcfval * pcfval

(* { param : string; body : expr; env : environment }
   fun v -> eval expr (extend env param v)
*)
and environment = (string * pcfval) list

(** Pretty-printer pour les valeurs *)
let rec printval oc = function
  | Intval n -> Printf.fprintf oc "%d" n
  | Floatval x -> Printf.fprintf oc "%f" x
  | Boolval b -> Printf.fprintf oc "%s" (if b then "true" else "false")
  | Stringval s -> Printf.fprintf oc "%S" s
  | Funval _ -> Printf.fprintf oc "<fun>"
  | Funrecval _ -> Printf.fprintf oc "<fun rec>"
  | Pairval (a, b) -> Printf.fprintf oc "(%a * %a)" printval a printval b

(* Environnement. *)
let init_env =
  [
    ( "squarert",
      Funval
        (function
        | Floatval f -> Floatval (Float.sqrt f) | _ -> failwith "unreachable")
    );
    ( "sqroot",
      Funval
        (function
        | Floatval f -> Floatval (Float.sqrt f) | _ -> failwith "unreachable")
    );
    ( "ln",
      Funval
        (function
        | Floatval f -> Floatval (Float.log f) | _ -> failwith "unreachable") );
    ( "exp",
      Funval
        (function
        | Floatval f -> Floatval (Float.exp f) | _ -> failwith "unreachable") );
    ( "sin",
      Funval
        (function
        | Floatval f -> Floatval (Float.sin f) | _ -> failwith "unreachable") );
    ( "cos",
      Funval
        (function
        | Floatval f -> Floatval (Float.cos f) | _ -> failwith "unreachable") );
    ( "tan",
      Funval
        (function
        | Floatval f -> Floatval (Float.log f) | _ -> failwith "unreachable") );
  ]

let error msg = raise (Failure msg)
let extend rho x v = (x, v) :: rho

let lookup var_name rho =
  try List.assoc var_name rho
  with Not_found -> error (Printf.sprintf "Undefined ident '%s'" var_name)

(** Evaluation des expressions pour leur donner un sens *)
let rec eval e rho =
  match e with
  | E_Int n -> Intval n
  | E_Float f -> Floatval f
  | E_Bool b -> Boolval b
  | E_String s -> Stringval s
  | E_Ident v -> lookup v rho
  | E_App (e1, e2) -> (
      match (eval e1 rho, eval e2 rho) with
      | Funval f, v2 -> f v2
      | (Funrecval { fname; param; body; env } as fval), v2 ->
          let rho1 = extend env fname fval in
          let rho2 = extend rho1 param v2 in
          eval body rho2
      | _, _ -> error "Apply a non-function")
  | E_Monop (op, e) -> (
      match op with
      | "-" -> (
          match eval e rho with
          | Intval n -> Intval (-n)
          | Floatval x -> Floatval (-.x)
          | _ -> error "Opposite of a non-number")
      | s -> error (Printf.sprintf "Unknown unary op: %s" s))
  | E_Binop (op, e1, e2) -> (
      match (op, eval e1 rho, eval e2 rho) with
      | "+", Intval n1, Intval n2 -> Intval (n1 + n2)
      | "-", Intval n1, Intval n2 -> Intval (n1 - n2)
      | "*", Intval n1, Intval n2 -> Intval (n1 * n2)
      | "/", Intval n1, Intval n2 -> Intval (n1 / n2)
      | "+", Floatval x1, Floatval x2 -> Floatval (x1 +. x2)
      | "-", Floatval x1, Floatval x2 -> Floatval (x1 -. x2)
      | "*", Floatval x1, Floatval x2 -> Floatval (x1 *. x2)
      | "/", Floatval x1, Floatval x2 -> Floatval (x1 /. x2)
      | ("+" | "-" | "*" | "/"), _, _ ->
          error "Arithmetic on non-integers or non-float"
      | "<", Intval n1, Intval n2 -> Boolval (n1 < n2)
      | ">", Intval n1, Intval n2 -> Boolval (n1 > n2)
      | "=", Intval n1, Intval n2 -> Boolval (n1 = n2)
      | "<=", Intval n1, Intval n2 -> Boolval (n1 <= n2)
      | ">=", Intval n1, Intval n2 -> Boolval (n1 >= n2)
      | ("<" | ">" | "=" | "<=" | ">="), _, _ ->
          error "Comparison of non-integers"
      | _ -> error (Printf.sprintf "Unknown binary op: %s" op))
  | E_If (e, e1, e2) -> (
      match eval e rho with
      | Boolval b -> eval (if b then e1 else e2) rho
      | _ -> error "Test on a non-boolean")
  | E_Fun (a, e) -> Funval (fun v -> eval e (extend rho a v))
  | E_Let (x, e1, e2) ->
      let v1 = eval e1 rho in
      let rho1 = extend rho x v1 in
      eval e2 rho1
  | E_Letrec (f, E_Fun (x, e1), e2) ->
      let fval = Funrecval { fname = f; param = x; body = e1; env = rho } in
      let rho1 = extend rho f fval in
      eval e2 rho1
  | E_Letrec (_, _, _) -> error "Cannot use the rec flag on non function value"
  | E_Pair (a, b) -> Pairval (eval a rho, eval b rho)
  | E_Tyannot (e, _) -> eval e rho

(** Evaluation wrapper top_def *)
let eval_sentence s rho =
  match s with
  | S_Expr e -> eval e rho
  | S_Let (_, e) -> eval e rho
  | S_Letrec (_, e) -> eval e rho

let eval e = eval e init_env
let eval_sentence s = eval_sentence s init_env
