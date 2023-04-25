type subst

val empty : subst
val singleton : Types.var_name -> Types.ty -> subst
val apply : Types.ty -> subst -> Types.ty
val subst_env : subst -> ('a * Types.sch) list -> ('a * Types.sch) list
val compose : subst -> subst -> subst
