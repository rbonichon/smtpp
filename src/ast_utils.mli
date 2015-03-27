val symbol_of_svar : Ast.sorted_var -> Ast.symbol ;;
  (** Extracts the symbol part out of the declaration of a sorted variable. *)

val symbol_of_vbinding : Ast.var_binding -> Ast.symbol ;;
  (** Extracts the newly defined symbol part out of a variable binding. *)

val mk_symbol : string -> Ast.symbol ;;
  (** [mk_symbol name] creates a dummy symbol for name *)

val get_logic : Ast.script -> string ;;
  (** Extracts the logic name from a SMT script *)
