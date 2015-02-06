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

(* Pretty printer for the AST *)
open Format
open Lexing
open Ast
;;

let _pp_loc fmt (b, e)  =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  fprintf fmt "L%d, C%d-%d" l fc lc
;;

let pp_list pp_f fmt elts =
  fprintf fmt "@[<hov 2>";
  let rec pp_list_aux fmt = function
    | [] -> ()
    | [e] -> fprintf fmt "%a" pp_f e
    | e :: es -> fprintf fmt "%a@ %a" pp_f e pp_list_aux es 
  in pp_list_aux fmt elts;
  fprintf fmt "@]";
;;

let pp_numeral fmt n = fprintf fmt "%n" n ;;

let pp_symbol fmt symb =
  match symb.symbol_desc with
  | SimpleSymbol s -> fprintf fmt "%s" s
  | QuotedSymbol s -> fprintf fmt "|%s|" s
;;

let pp_symbols fmt symbs = fprintf fmt "%a" (pp_list pp_symbol) symbs ;;

let pp_keyword fmt kwd = fprintf fmt ":%s" kwd ;;

let pp_spec_constant fmt = function
  | CstBool b -> fprintf fmt "%b" b
  | CstNumeral n -> fprintf fmt "%d" n
  | CstBinary str -> fprintf fmt "#b%s" str
  | CstDecimal str -> fprintf fmt "%s" str
  | CstHexadecimal str -> fprintf fmt "#x%s" str
  | CstString str -> fprintf fmt "\"%s\"" str
;;

let pp_idx fmt idx =
  match idx with
  | IdxNum n -> pp_numeral fmt n
  | IdxSymbol symb -> pp_symbol fmt symb
;;

let pp_indexes fmt indexes = pp_list pp_idx fmt indexes ;;

let pp_identifier fmt identifier =
  match identifier.id_desc with
  | IdSymbol symb -> pp_symbol fmt symb
  | IdUnderscore (symb, indexes) ->
     fprintf fmt "@[<hov 2>(_@ %a@ %a)@]"
             pp_symbol symb
             pp_indexes indexes
;;

let rec pp_sort fmt sort =
  match sort.sort_desc with
  | SortIdentifier id -> pp_identifier fmt id
  | SortFun (id, sorts) ->
     fprintf fmt "@[<hov 2>(%a@ %a)@]"
             pp_identifier id pp_sorts sorts

and pp_sorts fmt sorts = pp_list pp_sort fmt  sorts
;;

let pp_qual_identifier fmt qualid =
  match qualid.qual_identifier_desc with
  | QualIdentifierIdentifier id ->
     fprintf fmt "%a" pp_identifier id
  | QualIdentifierAs (id, sort) ->
     fprintf fmt "@[<hov 2>(as@ %a@ %a)@]"
             pp_identifier id
             pp_sort sort
;;


let pp_sorted_var fmt svar =
  match svar.sorted_var_desc with
  | SortedVar (symb, sort) ->
     fprintf fmt "@[<hov 2>(%a@ %a)@]" pp_symbol symb pp_sort sort
;;

let pp_sorted_vars fmt svars = fprintf fmt "%a" (pp_list pp_sorted_var) svars
;;

let rec pp_sexpr fmt sexpr =
  match sexpr.sexpr_desc with
  | SexprConstant sc -> pp_spec_constant fmt sc
  | SexprSymbol symb -> pp_symbol fmt symb
  | SexprKeyword kwd -> pp_keyword fmt kwd
  | SexprParens sexprs -> fprintf fmt "@[<hov 2>(%a)@]" pp_sexprs sexprs

and pp_sexprs fmt sexprs = pp_list pp_sexpr fmt sexprs
;;

let pp_attribute_value fmt attr_value =
  match attr_value.attr_value_desc with
  | AttrValSymbol symb -> pp_symbol fmt symb
  | AttrValSpecConstant sc -> pp_spec_constant fmt sc
  | AttrValSexpr sexprs -> fprintf fmt "@[<hov 2>(%a)@]" pp_sexprs sexprs
;;

let pp_attribute fmt attribute =
  match attribute.attribute_desc with
  | AttrKeyword kwd -> pp_keyword fmt kwd
  | AttrKeywordValue (kwd, value) ->
     fprintf fmt "%a %a" pp_keyword kwd pp_attribute_value value
;;

let pp_attributes fmt attributes = pp_list pp_attribute fmt attributes ;;

let rec pp_term fmt term =
  match term.term_desc with
  | TermSpecConstant sc -> pp_spec_constant fmt sc
  | TermQualIdentifier qualid -> pp_qual_identifier fmt qualid
  | TermQualIdentifierTerms (qualid, terms) ->
     fprintf fmt "@[<hov 2>(%a@ %a)@]"
             pp_qual_identifier qualid
             pp_terms terms
  | TermLetTerm (varbindings, term) ->
     fprintf fmt "@[<hov 2>(let@ (%a)@ %a)@]"
             pp_var_bindings varbindings
             pp_term term
  | TermForallTerm (svars, term) ->
     fprintf fmt "@[<hov 2>(forall@ (@[<hov>%a@])@ %a)@]"
             pp_sorted_vars svars
             pp_term term
  | TermExistsTerm (svars, term) ->
     fprintf fmt "@[<hov 2>(exists@ (@[<hov>%a@])@ %a)@]"
             pp_sorted_vars svars
             pp_term term
  | TermAnnotatedTerm (term, attrs) ->
     fprintf fmt "@[<hov 2>(!@ %a %a)@]"
             pp_term term
             pp_attributes attrs

and pp_terms fmt terms = fprintf fmt "%a" (pp_list pp_term) terms

and pp_var_binding fmt vbinding =
  match vbinding.var_binding_desc with
  | VarBinding (symb, term) ->
     fprintf fmt "(%a@ %a)" pp_symbol symb pp_term term

and pp_var_bindings fmt vbindings = pp_list pp_var_binding fmt vbindings ;;

let pp_opt_type_parameters fmt optsorts =
  match optsorts with
  | Some sorts -> fprintf fmt "par@ (%a)@ " pp_sorts sorts
  | None -> ()
;;


let pp_fun_def fmt fun_def =
  match fun_def.fun_def_desc with
  | FunDef (symb, optsorts , svars, sort, term) ->
     fprintf fmt "@[<hov>%a@ %a(%a)@ %a@ %a@]"
             pp_symbol symb
             pp_opt_type_parameters optsorts
             pp_sorted_vars svars
             pp_sort sort pp_term term


let pp_fun_rec_def fmt fun_rec_def =
  match fun_rec_def.fun_rec_def_desc with
  | FunRecDef (symb, optsorts, svars, sort, term) ->
     fprintf fmt "@[<hov>(%a@ %a(%a)@ %a@ %a)@]"
             pp_symbol symb
             pp_opt_type_parameters optsorts
             pp_sorted_vars svars
             pp_sort sort pp_term term
;;

let pp_fun_rec_defs fmt fun_rec_defs =
  fprintf fmt "@[<v 0>";
  List.iter (fun frdef -> pp_fun_rec_def fmt frdef) fun_rec_defs;
  fprintf fmt "@]";
;;

let pp_info_flag fmt info_flag =
  match info_flag.info_flag_desc with
  | InfoFlagKeyword kwd -> pp_keyword fmt kwd
;;

let pp_smt_option fmt smt_option =
  match smt_option.smt_option_desc with
  | OptionAttribute attr -> pp_attribute fmt attr
;;

let pp_cmd fmt cmd =
  match cmd.command_desc with
  | CmdAssert t ->
     fprintf fmt "@[<hov 2>assert@ %a@]" pp_term t
  | CmdCheckSat -> fprintf fmt "check-sat"
  | CmdCheckSatAssuming symbs ->
     fprintf fmt "check-sat-assuming@ (%a)" pp_symbols symbs
  | CmdDeclareConst (symb, sort) ->
     fprintf fmt "@[<hov 2>declare-const@ %a@ %a@]"
             pp_symbol symb
             pp_sort sort
  | CmdDeclareFun (symb, _, sorts, sort) ->
     fprintf fmt "declare-fun@ %a@ (%a)@ %a"
             pp_symbol symb
             pp_sorts sorts
             pp_sort sort
  | CmdDefineFun fundef ->
     fprintf fmt "define-fun@ %a" pp_fun_def fundef
  | CmdDefineFunRec funrecdefs ->
     fprintf fmt "define-fun@ (%a)" pp_fun_rec_defs funrecdefs
  | CmdDefineSort (symb, symbs, sort) ->
     fprintf fmt "define-sort@ %a@ (%a)@ %a"
             pp_symbol symb
             pp_symbols symbs
             pp_sort sort
  | CmdDeclareSort (symb, num) ->
     fprintf fmt "declare-sort@ %a@ %a"
             pp_symbol symb
             pp_numeral num
  | CmdGetAssertions -> fprintf fmt "get-assertions"
  | CmdGetAssignment -> fprintf fmt "get-assignment"
  | CmdExit -> fprintf fmt "exit"
  | CmdEcho s -> fprintf fmt "echo@ \"%s\"" s
  | CmdGetModel-> fprintf fmt "get-model"
  | CmdGetProof -> fprintf fmt "get-proof"
  | CmdGetUnsatCore -> fprintf fmt "get-unsat-core"
  | CmdGetInfo infoflag ->
     fprintf fmt "get-info@ %a" pp_info_flag infoflag
  | CmdGetOption kwd ->
     fprintf fmt "get-option@ %a" pp_keyword kwd
  | CmdGetUnsatAssumptions ->
     fprintf fmt "get-unsat-assumptions"
  | CmdGetValue terms ->
     fprintf fmt "get-value@ (%a)" pp_terms terms
  | CmdMetaInfo attr ->
     fprintf fmt "meta-info@ %a" pp_attribute attr
  | CmdPop (Some n) ->
     fprintf fmt "pop@ %a" pp_numeral n
  | CmdPush (Some n) ->
     fprintf fmt "push@ %a" pp_numeral n
  | CmdPop None ->
     fprintf fmt "pop" 
  | CmdPush None  ->
     fprintf fmt "push"
  | CmdReset ->
     fprintf fmt "reset"
  | CmdResetAssertions ->
     fprintf fmt "reset-assertions"
  | CmdSetInfo attr ->
     fprintf fmt "set-info@ %a" pp_attribute attr
  | CmdSetLogic symb ->
     fprintf fmt "set-logic@ %a" pp_symbol symb
  | CmdSetOption smt_option ->
     fprintf fmt "set-option@ %a" pp_smt_option smt_option
;;

let pp_command_list fmt l =
 (* All commands are delimited by (). This is printed here. *)
  List.iter (fun cmd -> Format.fprintf fmt "@[<hov 2>(%a)@]@ " pp_cmd cmd) l
;;

let pp_commands fmt cmds =
 Format.fprintf fmt "@[<v 0>%a@]@." pp_command_list cmds
;;

let pp fmt (s:script) = pp_commands fmt s.script_commands ;;

let pp_tofile filename program =
  let oc = Pervasives.open_out_bin filename in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." pp program;
  close_out oc;
;;
