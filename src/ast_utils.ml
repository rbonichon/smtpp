open Ast ;;

let symbol_of_svar (sv : sorted_var) =
  match sv.sorted_var_desc with
  | SortedVar (sy, _) -> sy
;;

let sort_of_svar (sv : sorted_var) =
  match sv.sorted_var_desc with
  | SortedVar (_, so) -> so
;;

let symbol_of_vbinding (vb : var_binding) =
  match vb.var_binding_desc with
  | VarBinding (sy, _) -> sy
;;

let id_from_qid (qid : Ast.qual_identifier) : identifier =
  match qid.qual_identifier_desc with
  | QualIdentifierAs (id, _)
  | QualIdentifierIdentifier id -> id
;;

let get_logic (s : Ast.script) =
  let rec aux (cmds : Ast.commands) =
    match cmds with
    | [] -> ""
    | { command_desc = CmdSetLogic symb; _ } :: _ ->
       begin
         match symb.symbol_desc with
           | SimpleSymbol logic_name -> logic_name
           | QuotedSymbol _ -> assert false
       end
    | _ :: cmds -> aux cmds
  in aux s.script_commands
;;

let rec is_constant_term (t : Ast.term) : bool =
  match t.term_desc with
  | TermSpecConstant _ -> true
  | TermAnnotatedTerm (t, _) -> is_constant_term t
  | TermLetTerm _
  | TermQualIdentifier _
  | TermQualIdentifierTerms _
  | TermForallTerm _
  | TermExistsTerm _ -> false
;;

let mk_symbol (s:string) =
  { symbol_desc = SimpleSymbol s;
    symbol_loc = Locations.dummy_loc;
  }
;;
