type subst

val empty : subst
val singleton : Types.var_name -> Types.ty_t -> subst
val apply : Types.ty_t -> subst -> Types.ty_t
val subst_env : subst -> ('a * Types.sch_t) list -> ('a * Types.sch_t) list
val compose : subst -> subst -> subst
