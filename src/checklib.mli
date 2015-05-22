val is_constant_term : Ast.term -> bool ;;
  (** [is_constant t] checks if the term t is a constant or not.
   *  A real constant might be hidden under an annotated term.
   *)

val is_variable_term : Ast.term -> bool ;;
  (** [is_variable t] checks if the term t is possibly a variable or not.
   *)
