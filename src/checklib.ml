open Ast ;;
open Ast_utils ;;

let rec is_constant_term (t : Ast.term) : bool =
  match t.term_desc with
  | TermSpecConstant _ -> true
  | TermAnnotatedTerm (t, _) -> is_constant_term t
  | TermLetTerm _
  | TermQualIdentifier _ -> false
  | TermQualIdentifierTerms (qid, terms) ->
     let symbol = symbol_of_id (id_of_qid qid) in
     if Theory.is_smtlib_symbol symbol then
       (* Hypothesis to verify: all SMTLIB function symbols are mathematical
        * functions *)
       List.for_all is_constant_term terms
     else false
  | TermForallTerm _
  | TermExistsTerm _ -> false
;;

let is_variable_term (t : Ast.term) : bool =
  not (is_constant_term t)
;;
