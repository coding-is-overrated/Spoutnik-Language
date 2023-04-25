exception Error of string
type var_name = string
type ty =
    TInt
  | TBool
  | TString
  | TFun of (ty * ty)
  | TVar of var_name
  | TPair of (ty * ty)

type sch = ((var_name list) * ty)
type env = (string * sch) list

val init_env : env
val new_ty_var : unit -> ty
val instance : var_name list * ty -> ty
val trivial_sch : ty -> sch
val generalize : ty -> ('a * sch) list -> sch
val print : out_channel -> ty -> unit
