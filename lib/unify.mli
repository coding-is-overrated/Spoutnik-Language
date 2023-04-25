exception Cycle of (Types.var_name * Types.ty)
exception Conflict of (Types.ty * Types.ty)
val occur_check : Types.var_name -> Types.ty -> bool
val unify : Types.ty -> Types.ty -> Subst.subst
