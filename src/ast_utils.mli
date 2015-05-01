val symbol_of_svar : Ast.sorted_var -> Ast.symbol ;;
  (** Extracts the symbol part out of the declaration of a sorted variable. *)

val sort_of_svar : Ast.sorted_var -> Ast.sort ;;
  (** Extracts the sort part out of the declaration of a sorted variable. *)

val symbol_of_vbinding : Ast.var_binding -> Ast.symbol ;;
  (** Extracts the newly defined symbol part out of a variable binding. *)

val symbols_of_sort : Ast.sort ->  Ast.symbol list ;;
val string_of_symbol : Ast.symbol -> string ;;

val mk_symbol : string -> Ast.symbol ;;
  (** [mk_symbol name] creates a dummy symbol for name *)

val mk_localized_symbol : string -> Locations.t -> Ast.symbol ;;

val get_logic : Ast.script -> string ;;
  (** Extracts the logic name from a SMT script *)

val id_from_qid : Ast.qual_identifier -> Ast.identifier ;;
  (** [id_from_qid qid] extracts the id part of a qualified identifier *)

val is_constant_term : Ast.term -> bool ;;
  (** [is_constant t] checks if the term t is a constant or not.
   *  A real constant might be hidden under an annotated term.
   *)

val is_variable_term : Ast.term -> bool ;;
  (** [is_variable t] checks if the term t is possibly a variable or not.
   *)
