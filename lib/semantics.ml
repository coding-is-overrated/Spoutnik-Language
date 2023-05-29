open Ast

(** Définition des types *)
type value =
  | Intval of int
  | Floatval of float
  | Boolval of bool
  | Stringval of string
  | Funval of (value -> value)
  | Pairval of value * value

and environment = (string * value) list

(** Pretty-printer pour les valeurs *)
let rec printval oc = function
  | Intval n -> Printf.fprintf oc "%d" n
  | Floatval x -> Printf.fprintf oc "%F" x
  | Boolval b -> Printf.fprintf oc "%s" (if b then "true" else "false")
  | Stringval s -> Printf.fprintf oc "%S" s
  | Funval _ -> Printf.fprintf oc "<fun>"
  | Pairval (a, b) -> Printf.fprintf oc "(%a * %a)" printval a printval b

(** Environnement initial auquel on a rajouté la librairie standard *)
let init_env =
  [
    ( "sqrt",
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
        | Floatval f -> Floatval (Float.tan f) | _ -> failwith "unreachable") );
    ( "print",
      Funval
        (fun v ->
          Printf.printf "%a\n%!" printval v;
          Intval 0) );
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
      | "=", n1, n2 -> Boolval (n1 = n2)
      | "<=", Intval n1, Intval n2 -> Boolval (n1 <= n2)
      | ">=", Intval n1, Intval n2 -> Boolval (n1 >= n2)
      | ("<" | ">" | "<=" | ">="), _, _ -> error "Comparison of non-integers"
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
  | E_Letrec (f_name, E_Fun (x, e1), e2) ->
      let rec f v = eval e1 (extend (extend rho f_name (Funval f)) x v) in
      let rho1 = extend rho f_name (Funval f) in
      eval e2 rho1
  | E_Letrec (_, _, _) -> error "Cannot use the rec flag on non function value"
  | E_Pair (a, b) -> Pairval (eval a rho, eval b rho)
  | E_Tyannot (e, _) -> eval e rho

(** Evaluation wrapper top_def *)
let eval_sentence s rho =
  match s with
  | S_Expr e -> eval e !rho
  | S_Let (x, e) ->
      let v = eval e !rho in
      rho := extend !rho x v;
      v
  | S_Letrec (f_name, e) ->
      let v = eval (E_Letrec (f_name, e, E_Ident f_name)) !rho in
      rho := extend !rho f_name v;
      v

let eval e = eval e init_env
