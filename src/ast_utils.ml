open Ast ;;

let symbol_of_svar (sv : sorted_var) =
  match sv.sorted_var_desc with
  | SortedVar (sy, _) -> sy
;;

let symbol_of_vbinding (vb : var_binding) =
  match vb.var_binding_desc with
  | VarBinding (sy, _) -> sy
;;
