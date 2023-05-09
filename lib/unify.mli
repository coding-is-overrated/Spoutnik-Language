exception Cycle of (Types.var_name * Types.ty_t)
exception Conflict of (Types.ty_t * Types.ty_t)
val occur_check : Types.var_name -> Types.ty_t -> bool
val unify : Types.ty_t -> Types.ty_t -> Subst.subst
