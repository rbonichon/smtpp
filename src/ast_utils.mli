val symbol_of_svar : Ast.sorted_var -> Ast.symbol ;;
  (** Extracts the symbol part out of the declaration of a sorted variable. *)

val symbol_of_vbinding : Ast.var_binding -> Ast.symbol ;;
  (** Extracts the newly defined symbol part out of a variable binding. *)
