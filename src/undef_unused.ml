(**
 ** Computation of  undefined and unused variables.
 ** If a name has multiple types, any occurrence of the name will mark all of
 ** its types as used (no type distinction is made).
 *)

open Ast
open Utils
;;

type varstate = {
    used : SymbolSet.t ;
    defined : SymbolSet.t
  }
;;

let empty_vstate = { used = SymbolSet.empty; defined = SymbolSet.empty; } ;;

let define vs sy =
  { vs with defined = SymbolSet.add sy vs.defined; }
;;

let use vs sy =
  { vs with used = SymbolSet.add sy vs.used; }
;;

let pp_symbols fmt (sys : SymbolSet.t) =
  SymbolSet.iter
    (fun sy -> Format.fprintf fmt "%a;@ " Pp.pp_symbol sy) sys
;;

let svar_set (svars: Ast.sorted_vars) =
  List.fold_left
    (fun s sv -> SymbolSet.add (Ast_utils.symbol_of_svar sv) s)
    SymbolSet.empty svars
;;

let eval_index (vs : varstate) (idx : Ast.index) =
  match idx with
  | IdxNum _ -> vs
  | IdxSymbol sy -> use vs sy
;;

let eval_identifier (vs : varstate) (id : Ast.identifier) =
  match id.id_desc with
  | IdSymbol sy -> use vs sy
  | IdUnderscore (sy, idxs) -> use (List.fold_left eval_index vs idxs) sy
;;

let eval_qual_identifier (vs : varstate) (qid : Ast.qual_identifier) =
  match qid.qual_identifier_desc with
  | QualIdentifierAs (id, _)
  | QualIdentifierIdentifier id -> eval_identifier vs id
;;

let rec eval_term (vs: varstate) (t: Ast.term) =
  match t.term_desc with
  | TermSpecConstant _ -> vs
  | TermQualIdentifier qid -> eval_qual_identifier vs qid
  | TermQualIdentifierTerms (qid, terms) ->
     eval_qual_identifier (eval_terms vs terms) qid
  | TermLetTerm (vbindings, term) ->
     let vsbindings = List.fold_left eval_var_binding empty_vstate vbindings in
     let vs' = { defined = SymbolSet.union vsbindings.defined vs.defined;
                 used = SymbolSet.empty; } in
     let vsterm = eval_term vs' term in
     let outside_used = SymbolSet.diff vsterm.used vsbindings.defined in
     let unused_bindings = SymbolSet.diff vsbindings.defined vsterm.used in
     if unused_bindings <> SymbolSet.empty then
       Format.printf
         "@[<v 0>Unused bounded variables: @[<hov 0>%a@]@]@."
         pp_symbols unused_bindings;
     { vs with used = SymbolSet.union outside_used vsbindings.used; }
  | TermForallTerm (svars, term)
  | TermExistsTerm (svars, term) ->
     let new_symbols = svar_set svars in
     let defined = SymbolSet.union new_symbols vs.defined in
     let vsterm = eval_term { empty_vstate with defined; } term in
     let outside_used = SymbolSet.diff vsterm.used new_symbols in
     { vs with used = SymbolSet.union outside_used vs.used; }
  | TermAnnotatedTerm (term, _) -> eval_term vs term

and eval_var_binding (vs : varstate) (vb : Ast.var_binding) =
  match vb.var_binding_desc with
  | VarBinding (sy, term) -> define (eval_term vs term) sy

and eval_terms vs terms =
  List.fold_left eval_term vs terms
;;

let eval_fundef (vs : varstate) (f : Ast.fun_def) =
  match f.fun_def_desc with
  | FunDef (sy, _, svars, _, term) ->
     let new_symbols = svar_set svars in
     let fdefined = SymbolSet.union vs.defined new_symbols in
     let vs' =
       eval_term { defined = fdefined; used = SymbolSet.empty; } term in
     let unused_params = SymbolSet.diff fdefined vs'.used in
     if unused_params <> SymbolSet.empty then
       Format.printf
         "@[<v 0>Unused parameters for function %a:@ @[<hov 0>%a@]@]@."
         Pp.pp_symbol sy
         pp_symbols unused_params;
     let outer_scope_used = SymbolSet.diff vs'.used fdefined in
     let used' = SymbolSet.union vs.used outer_scope_used in
     define { vs with used = used' } sy
;;

let eval_command (vs : varstate) (cmd : Ast.command) =
  match cmd.command_desc with
  | CmdAssert t -> eval_term vs t
  | CmdDeclareConst (sy, _)
  | CmdDeclareFun (sy, _, _, _) -> define vs sy
  | CmdDefineFun fdef -> eval_fundef vs fdef
  | CmdCheckSatAssuming symbols -> List.fold_left use vs symbols
  | CmdDefineFunRec _fdef_rec -> Io.not_yet_implemented "Recursive function"
  | CmdCheckSat
  | CmdDeclareSort _
  | CmdDefineSort _
  | CmdEcho _
  | CmdExit
  | CmdGetAssertions
  | CmdGetAssignment
  | CmdGetInfo _
  | CmdGetModel
  | CmdGetOption _
  | CmdGetProof
  | CmdGetUnsatAssumptions
  | CmdGetUnsatCore
  | CmdGetValue _
  | CmdMetaInfo _
  | CmdPop _
  | CmdPush _
  | CmdReset
  | CmdResetAssertions
  | CmdSetInfo _
  | CmdSetLogic _
  | CmdSetOption _ -> vs
;;

let eval_commands vs cmds =
  List.fold_left eval_command vs cmds
;;

let apply (script: Ast.script) =
  let vstate = { used = SymbolSet.empty; defined = SymbolSet.empty; } in
  let vs = eval_commands vstate script.script_commands in
  let unused = SymbolSet.diff vs.defined vs.used in
  let undefined = SymbolSet.diff vs.used vs.defined in
  let pp_set (title : string) (s : SymbolSet.t) =
    if s <> SymbolSet.empty then
      Format.printf
        "@[<v 0>%a%a@]"
        Utils.mk_header title
        pp_symbols s
  in
  pp_set "Unused symbols" unused;
  pp_set "Undefined symbols" undefined;
;;
