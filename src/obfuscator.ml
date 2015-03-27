(**************************************************************************)
(*  Copyright (c) 2015 Richard Bonichon <richard.bonichon@gmail.com>      *)
(*                                                                        *)
(*  Permission to use, copy, modify, and distribute this software for any  *)
(*  purpose with or without fee is hereby granted, provided that the above  *)
(*  copyright notice and this permission notice appear in all copies.     *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  *)
(*  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF      *)
(*  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *)
(*  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES  *)
(*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN  *)
(*  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *)
(*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.        *)
(*                                                                        *)
(**************************************************************************)

open Ast ;;
open Extended_ast ;;
open Theory ;;
open Format ;;

module HashedSymb = struct
    type t = Ast.symbol ;;

    let equal sy1 sy2 =
      match sy1.symbol_desc, sy2.symbol_desc with
      | SimpleSymbol s1, SimpleSymbol s2
      | QuotedSymbol s1, QuotedSymbol s2 -> String.compare s1 s2 = 0
      | _, _ -> false
    ;;

    let hash sy = Hashtbl.hash sy.symbol_desc ;;
  end
;;

module SymbHash = struct
   include Hashtbl.Make(HashedSymb)

   let h = create 97 ;;
   let n = ref (-1) ;;
   let base = ref "S" ;;

   let init ?keep:(keep = []) () =
     List.iter (fun s -> let sy = Ast_utils.mk_symbol s in add h sy sy) keep;
   ;;

   let get_symbol symb =
     try
       let sy = find h symb in
       sy
     with
     | Not_found ->
        begin
          incr n;
          let newsymb =
            { symb with symbol_desc = SimpleSymbol (!base ^ (string_of_int !n)) }
          in
          add h symb newsymb;
          newsymb
        end
   ;;

   let get_htable () = h ;;
end
;;



let obfuscate_index = function
  | IdxNum n -> IdxNum n
  | IdxSymbol symb -> IdxSymbol (SymbHash.get_symbol symb)
;;

let obfuscate_indexes = List.map obfuscate_index ;;

let obfuscate_id id =
  let id_desc =
    match id.id_desc with
    | IdSymbol symb -> IdSymbol (SymbHash.get_symbol symb)
    | IdUnderscore (symb, indexes) ->
       IdUnderscore (SymbHash.get_symbol symb, obfuscate_indexes indexes)
  in { id with id_desc }
;;

let obfuscate_qid qid =
  let qual_identifier_desc =
    match qid.qual_identifier_desc with
    | QualIdentifierIdentifier id -> QualIdentifierIdentifier (obfuscate_id id)
    | QualIdentifierAs (id, sort) -> QualIdentifierAs (obfuscate_id id, sort)
  in { qid with qual_identifier_desc }
;;

let rec obfuscate_sexpr sexpr =
  let sexpr_desc =
    match sexpr.sexpr_desc with
    | SexprConstant _ | SexprKeyword _ as sdesc -> sdesc
    | SexprSymbol symb -> SexprSymbol (SymbHash.get_symbol symb)
    | SexprParens sexprs -> SexprParens (obfuscate_sexprs sexprs)
  in { sexpr with sexpr_desc }

and obfuscate_sexprs terms = List.map obfuscate_sexpr terms ;;

let obfuscate_attr_value avalue =
  let attr_value_desc =
    match avalue.attr_value_desc with
    | AttrValSpecConstant c ->  AttrValSpecConstant c
    | AttrValSymbol symb -> AttrValSymbol (SymbHash.get_symbol symb)
    | AttrValSexpr sexprs -> AttrValSexpr (obfuscate_sexprs sexprs)
  in { avalue with attr_value_desc }
;;

let obfuscate_attribute attr =
  let attribute_desc =
    match attr.attribute_desc with
    | AttrKeyword kwd -> AttrKeyword kwd
    | AttrKeywordValue (kwd, attr_value) ->
       AttrKeywordValue (kwd, obfuscate_attr_value attr_value)
  in { attr with attribute_desc }
;;

let obfuscate_attributes = List.map obfuscate_attribute ;;

let obfuscate_sorted_var svar =
  let sorted_var_desc =
    match svar.sorted_var_desc with
    | SortedVar (symb, sort) -> SortedVar (SymbHash.get_symbol symb, sort)
  in { svar with sorted_var_desc }
;;

let obfuscate_sorted_vars = List.map obfuscate_sorted_var  ;;

let rec obfuscate_term term =
  let term_desc =
    match term.term_desc with
    | TermSpecConstant _ -> term.term_desc
    | TermQualIdentifier qid -> TermQualIdentifier (obfuscate_qid qid)
    | TermQualIdentifierTerms (qid, terms) ->
       TermQualIdentifierTerms (obfuscate_qid qid, obfuscate_terms terms)
    | TermLetTerm (vbindings, term) ->
       TermLetTerm (obfuscate_vbindings vbindings, obfuscate_term term)
    | TermForallTerm (sortedvars, term) ->
       TermForallTerm (obfuscate_sorted_vars sortedvars, obfuscate_term term)
    | TermExistsTerm (sortedvars, term) ->
       TermExistsTerm (obfuscate_sorted_vars sortedvars, obfuscate_term term)
    | TermAnnotatedTerm (term, attrs) ->
       TermAnnotatedTerm (obfuscate_term term, obfuscate_attributes attrs)
  in { term with term_desc }

and obfuscate_terms terms = List.map obfuscate_term terms

and obfuscate_vbinding vbinding =
  let var_binding_desc =
    match vbinding.var_binding_desc with
    | VarBinding (symb, term) ->
       VarBinding (SymbHash.get_symbol symb, obfuscate_term term)
  in { vbinding with var_binding_desc }

and obfuscate_vbindings vbindings = List.map obfuscate_vbinding vbindings
;;

let obfuscate_fun_def fdef =
  let fun_def_desc =
    match fdef.fun_def_desc with
    | FunDef (symb, par, vars, sort, t) ->
       let s = SymbHash.get_symbol symb in
       FunDef (s, par, obfuscate_sorted_vars vars, sort, obfuscate_term t)
  in { fdef with fun_def_desc }
;;

let obfuscate_fun_rec_def frecdec =
  let fun_rec_def_desc =
    match frecdec.fun_rec_def_desc with
    | FunRecDef (symb, par, vars, sort, t) ->
        let s = SymbHash.get_symbol symb in
        FunRecDef (s, par, obfuscate_sorted_vars vars, sort, obfuscate_term t)
  in { frecdec with fun_rec_def_desc }
;;

let obfuscate_opt opt =
  let smt_option_desc =
    match opt.smt_option_desc with
    | OptionAttribute attr -> OptionAttribute (obfuscate_attribute attr)
  in { opt with smt_option_desc }
;;

let obfuscate_command cmd =
  let command_desc =
    match cmd.command_desc with
    | CmdSetLogic _
    | CmdReset
    | CmdResetAssertions
    | CmdCheckSat
    | CmdEcho _
    | CmdExit
    | CmdGetAssertions
    | CmdGetModel
    | CmdGetAssignment
    | CmdGetProof
    | CmdGetUnsatCore
    | CmdGetUnsatAssumptions
    | CmdPop _
    | CmdPush _
    | CmdGetOption _
    | CmdGetInfo _ as c -> c
    | CmdAssert term ->
       CmdAssert (obfuscate_term term)
    | CmdDeclareConst (symb, sort)  ->
        let s = SymbHash.get_symbol symb in
       CmdDeclareConst (s, sort)
    | CmdDeclareFun (symb, par, dom, codom) ->
        let s = SymbHash.get_symbol symb in
       CmdDeclareFun (s, par, dom, codom)
    | CmdDefineFun fdef ->
        CmdDefineFun(obfuscate_fun_def fdef)
    | CmdDefineFunRec frecdeflist ->
       CmdDefineFunRec (List.map obfuscate_fun_rec_def frecdeflist)
    | CmdCheckSatAssuming symbs ->
       CmdCheckSatAssuming (List.map SymbHash.get_symbol symbs)
    | CmdDeclareSort (symb, num) ->
       (* Should we obfuscate declared sort symbols ? *)
       CmdDeclareSort (symb, num)
    | CmdDefineSort (symb, symbs, sort) ->
       (* Should we obfuscate declared sort symbols ? *)
       CmdDefineSort(symb, symbs, sort)
    | CmdGetValue terms ->
       CmdGetValue (obfuscate_terms terms)
    | CmdMetaInfo attr ->
       CmdMetaInfo (obfuscate_attribute attr)
    | CmdSetInfo attr ->
       CmdSetInfo (obfuscate_attribute attr)
    | CmdSetOption opt -> CmdSetOption (obfuscate_opt opt)

  in { cmd with command_desc }
;;

let obfuscate_commands cmds = List.map obfuscate_command cmds ;;

let apply (script : Extended_ast.script) =
  let theory_keeps = List.map fst script.script_theory.theory_symbols in
  let keep = (Config.get_keep_symbols ()) @ theory_keeps in
  SymbHash.init ~keep (); (* Init hash table with symbols that should be kept *)
  let script_commands = obfuscate_commands script.script_commands in
  let obfuscated_script = { script with script_commands } in
  printf "%a" Pp.pp_extended obfuscated_script;
  if Config.get_debug () then
    printf "@[<v 0>%a@ %a@]"
           Utils.mk_header "Symbol table"
           (fun fmt h ->
            SymbHash.iter
              (fun k v ->
               Format.fprintf fmt "%a -> %a@ " Pp.pp_symbol k Pp.pp_symbol v)
              h) (SymbHash.get_htable ());
;;
