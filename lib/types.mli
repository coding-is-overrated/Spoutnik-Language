exception Error of string
type var_name = string

type unit_t =
  | UVar of var_name
  | UBase of string
  | UOne
  | UProd of (unit_t * unit_t)
  | UDiv of (unit_t * unit_t)
  | UPow of (unit_t * int)

type ty_t =
  | TVar of var_name
  | TBase of  (string * unit_t)
  | TFun of (ty_t * ty_t)
  | TPair of (ty_t * ty_t)

val type_int : unit -> ty_t
val type_bool : unit -> ty_t
val type_string : unit -> ty_t
val type_pair : ty_t -> ty_t -> ty_t
val type_var : unit -> ty_t
val type_fun : ty_t -> ty_t -> ty_t
val type_basic : string -> unit_t -> ty_t

type sch_t = ((var_name list) * ty_t)
type env_t = (string * sch_t) list

val init_env : env_t ref
val instance : var_name list * ty_t -> ty_t
val trivial_sch : ty_t -> sch_t
val generalize : ty_t -> ('a * sch_t) list -> sch_t
val print : out_channel -> ty_t -> unit
