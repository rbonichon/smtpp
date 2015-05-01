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
open Logic ;;

module BasicInference (T : Theory.Theory) = struct
    exception Detected of Locations.t ;;

    let check_symbol (sy : Ast.symbol) =
      if T.mem sy then raise (Detected sy.symbol_loc);
    ;;

    let check_identifier (id : Ast.identifier) =
      match id.id_desc with
      | IdSymbol sy -> check_symbol sy
      | IdUnderscore (sy, _idx) -> check_symbol sy
    ;;

    let rec check_sort (sort : Ast.sort) =
      match sort.sort_desc with
      | SortIdentifier id -> check_identifier id
      | SortFun (id, sorts) ->
         check_identifier id;
         List.iter check_sort sorts;
    ;;

    let rec check_term (term : Ast.term) =
      match term.term_desc with
      | TermSpecConstant _
      | TermQualIdentifier _ -> ()
      | TermQualIdentifierTerms (_, terms) -> List.iter check_term terms
      | TermLetTerm (vbindings, term) ->
         List.iter check_var_binding vbindings;
         check_term term
      | TermForallTerm (svars, term)
      | TermExistsTerm (svars, term) ->
         List.iter check_sort (List.map Ast_utils.sort_of_svar svars);
         check_term term
      | TermAnnotatedTerm (term, _) -> check_term term

    and check_var_binding (vb : Ast.var_binding) =
      match vb.var_binding_desc with
      | VarBinding (_, term) -> check_term term
    ;;

    let check_fun_decl svars rsort body =
      let sortnames = rsort :: (List.map Ast_utils.sort_of_svar svars) in
      List.iter check_sort sortnames;
      check_term body;
    ;;

    let check_fun_def (fdef : Ast.fun_def) =
      match fdef.fun_def_desc with
      | FunDef (_, _, svars, rsort, body) -> check_fun_decl svars rsort body
    ;;

    let check_fun_rec_def (frdef : Ast.fun_rec_def) =
      match frdef.fun_rec_def_desc with
      | FunRecDef (_, _, svars, rsort, body) -> check_fun_decl svars rsort body
    ;;

    let check_command (cmd : Ast.command) =
      match cmd.command_desc with
      | CmdAssert term -> check_term term
      | CmdDefineFun fdef -> check_fun_def fdef
      | CmdDefineFunRec frdefs -> List.iter check_fun_rec_def frdefs
      | CmdDeclareFun (_, _, sorts, rsort) ->
         List.iter check_sort (rsort :: sorts)
      | CmdDefineSort (_, _, sort) -> check_sort sort
      | CmdDeclareSort (sy, _) -> check_symbol sy
      | CmdCheckSat
      | CmdCheckSatAssuming _
      | CmdDeclareConst _
      | CmdEcho _
      | CmdExit
      | CmdGetInfo _
      | CmdGetModel
      | CmdGetOption _
      | CmdGetProof
      | CmdGetUnsatAssumptions
      | CmdGetUnsatCore
      | CmdMetaInfo _
      | CmdPop _
      | CmdPush _
      | CmdReset
      | CmdResetAssertions
      | CmdSetInfo _
      | CmdSetLogic _
      | CmdGetAssertions
      | CmdGetAssignment
      | CmdSetOption _ -> ()
      | CmdGetValue terms -> List.iter check_term terms
    ;;

    let check_script (s : Ast.script) =
      List.iter check_command s.script_commands
    ;;

    let use_theory (s : Ast.script) =
      try check_script s; false with
        Detected loc ->
        Io.debug "%s : detected at %a@." T.name Pp.pp_loc loc;
        true ;;
end

module UF = struct
    exception FoundUF of string;;
    open Ast_utils ;;

    let is_abstract sort =
      let has_abstract_sortname (sortname : string) =
        match sortname with
        | "Int" | "Real"
        | "Bool"
        | "Array" | "BitVec"
        | "Index" | "Element" -> false
        | _ -> true
      in
      let sort_symbols = symbols_of_sort sort in
      match sort_symbols with
      | sortsymbol :: _sortnames ->
         has_abstract_sortname (string_of_symbol sortsymbol)
      | [] -> assert false
     ;;

     let check_command cmd =
       match cmd.command_desc with
       | CmdDeclareFun (_, _, sorts, sort) ->
          if List.length sorts > 0 then
            let msg = Utils.sfprintf "declare-fun %a" Pp.pp_loc cmd.command_loc
            in raise (FoundUF msg)
          else if is_abstract sort then
            let msg = Utils.sfprintf "abstract-sort %a" Pp.pp_loc sort.sort_loc
            in raise (FoundUF msg);
(*       | CmdDefineFun _
       | CmdDefineFunRec _ ->
          let msg = Utils.sfprintf "define-fun %a" Pp.pp_loc cmd.command_loc in
          raise (FoundUF msg);
 *)
       | CmdDeclareConst (_, sort) ->
          let msg = Utils.sfprintf "abstract-sort %a" Pp.pp_loc sort.sort_loc in
          if is_abstract sort then raise (FoundUF msg);
       | _ -> ()
    ;;

    let has_uninterpreted_functions (s : Ast.script) =
      try
        List.iter check_command s.script_commands;
        false
      with FoundUF msg -> Io.debug "UF : %s@." msg; true
    ;;
end

module QF = struct
    exception FoundQ ;;

    let rec check_var_binding vbinding =
      match vbinding.var_binding_desc with
      | VarBinding (_, term) -> check_term term

    and check_term term =
      match term.term_desc with
     | TermSpecConstant _
     | TermQualIdentifier _ -> ()
     | TermForallTerm _
     | TermExistsTerm _ -> raise FoundQ
     | TermQualIdentifierTerms (_, terms) -> List.iter check_term terms
     | TermAnnotatedTerm (term, _) -> check_term term
     | TermLetTerm (vbindings, term) ->
        List.iter check_var_binding vbindings;
        check_term term
    ;;

    let check_fun_def fdef =
      match fdef.fun_def_desc with
      | FunDef (_, _, _, _, term) -> check_term term
    ;;

    let check_fun_rec_def fdef =
      match fdef.fun_rec_def_desc with
      | FunRecDef (_, _, _, _, term) -> check_term term
    ;;

    let check_command cmd =
      match cmd.command_desc with
      | CmdAssert term -> check_term term
      | CmdDefineFun fdef -> check_fun_def fdef
      | CmdDefineFunRec frdefs -> List.iter check_fun_rec_def frdefs
      | CmdCheckSat
      | CmdCheckSatAssuming _
      | CmdDeclareConst _
      | CmdDeclareSort _
      | CmdDeclareFun _
      | CmdDefineSort _
      | CmdEcho _
      | CmdExit
      | CmdGetInfo _
      | CmdGetModel
      | CmdGetOption _
      | CmdGetProof
      | CmdGetUnsatAssumptions
      | CmdGetUnsatCore
      | CmdMetaInfo _
      | CmdPop _
      | CmdPush _
      | CmdReset
      | CmdResetAssertions
      | CmdSetInfo _
      | CmdSetLogic _
      | CmdGetAssertions
      | CmdGetAssignment
      | CmdSetOption _ -> ()
      | CmdGetValue terms -> List.iter check_term terms
    ;;

    let check_script (s : Ast.script) =
      List.iter check_command s.script_commands
    ;;

    let has_quantifier (s : Ast.script) =
      try check_script s; false with FoundQ -> true
    ;;
end


module ArithmeticCheck = struct
    open Utils ;;
    open Theory ;;

    type ternary =
      | True
      | Maybe
      | False
    ;;

    let is_unset (v : ternary) =
      match v with
      | True -> false
      | Maybe | False -> true
    ;;

    type result = {
        has_int : ternary;
        has_real : ternary;
        kind : Logic.arith_kind option;
      }
    ;;

    let int_symbols =
      let sorts, functions = all_symbol_strings SMTInt.theory in
      Utils.StringSet.union sorts functions
    ;;

    let real_symbols =
      let sorts, functions = all_symbol_strings SMTReal.theory in
      Utils.StringSet.union sorts functions
    ;;

    let kmap =
      StringMap.merge
        (fun _ x y ->
         match x, y with
         | Some x1, Some y1 -> assert (x1 = y1); Some x1
         | Some x, None -> Some x
         | None, Some y -> Some y
         | None, None -> None
        ) SMTInt.kind_map SMTReal.kind_map
    ;;

    let check_symbol (r : result) symb =
      match symb.symbol_desc with
      | SimpleSymbol sname ->
         begin
           let in_int = StringSet.mem sname int_symbols in
           let in_real = StringSet.mem sname real_symbols in
           match in_int, in_real with
           | true, true ->
              let has_int =
                match r.has_int with
                | False -> Maybe
                | Maybe | True -> r.has_int in
              let has_real =
                match r.has_real with
                | False -> Maybe
                | Maybe | True -> r.has_real in
              let kind =
                try Logic.max_kind_opt (Some (StringMap.find sname kmap)) r.kind
                with Not_found -> r.kind
              in { has_int; has_real; kind; }
           | true, false ->
              Io.debug "Arith: int symbol at %a@." Pp.pp_loc symb.symbol_loc;
              let kind =
                try Logic.max_kind_opt (Some (StringMap.find sname kmap)) r.kind
                with Not_found -> r.kind in
              { r with has_int = True; kind; }
           | false, true ->
              Io.debug "Arith: real symbol at %a@." Pp.pp_loc symb.symbol_loc;
              let kind =
                try Logic.max_kind_opt (Some (StringMap.find sname kmap)) r.kind
                with Not_found -> r.kind in
              { r with has_real = True; kind; }
           | false, false -> r
         end
      | QuotedSymbol _ -> r
    ;;

    let check_identifier (r : result) id =
      match id.id_desc with
      | IdSymbol sy -> check_symbol r sy
      | IdUnderscore _ -> r

    let rec check_sort (r : result) sort =
      match sort.sort_desc with
      | SortIdentifier id -> check_identifier r id
      | SortFun (id, sorts) ->
         List.fold_left check_sort (check_identifier r id) sorts

    let check_qual_identifier (r : result) qid =
      match qid.qual_identifier_desc with
        | QualIdentifierIdentifier id -> check_identifier r id
        | QualIdentifierAs (id, sort) -> check_sort (check_identifier r id) sort
    ;;

    let rec check_term (r : result) term =
      match term.term_desc with
      | TermSpecConstant _ -> r
      | TermQualIdentifier qid -> check_qual_identifier r qid
      | TermQualIdentifierTerms (qid, terms) ->
         begin
           match r.kind with
           | Some NonLinear -> r
           | _ ->
              let id = Ast_utils.id_from_qid qid in
              let r =
                if Theory.CommonArithmetics.is_multiplication id &&
                     Utils.has_more_than 1 Ast_utils.is_variable_term terms
                then begin
                    Io.debug "Non-linear multiplication detected on %a@."
                             Pp.pp_term term;
                    (* We have identified a case of multiplication which has more than
                     * one variable *)
                    { r with kind = Some NonLinear; }
                  end
                else r
              in List.fold_left check_term (check_qual_identifier r qid) terms
         end
      | TermLetTerm (vbindings, term) ->
         List.fold_left check_var_binding (check_term r term) vbindings
      | TermForallTerm (svars, term)
      | TermExistsTerm (svars, term) ->
         let varsorts = List.map Ast_utils.sort_of_svar svars in
         List.fold_left check_sort (check_term r term) varsorts
      | TermAnnotatedTerm (term, _) -> check_term r term

    and check_var_binding (r : result) vb =
      match vb.var_binding_desc with
      | VarBinding (_, term) -> check_term r term
    ;;

    let check_fun_decl r svars rsort (body : term) =
      let varsorts = List.map Ast_utils.sort_of_svar svars in
      List.fold_left check_sort (check_term r body) (rsort :: varsorts)
    ;;

    let check_fun_def (r : result) fdef =
      match fdef.fun_def_desc with
      | FunDef (_, _, svars, sort, term) -> check_fun_decl r svars sort term
    ;;

    let check_fun_rec_def (r : result) frdef =
      match frdef.fun_rec_def_desc with
      | FunRecDef (_, _, svars, sort, term) ->
         check_fun_decl r svars sort term
    ;;

    let check_command (r : result) cmd =
      match cmd.command_desc with
      | CmdAssert term -> check_term r term
      | CmdCheckSat
      | CmdDeclareSort _
      | CmdExit
      | CmdGetAssertions
      | CmdGetAssignment
      | CmdGetInfo _
      | CmdGetModel
      | CmdGetOption _
      | CmdGetProof
      | CmdGetUnsatAssumptions
      | CmdGetUnsatCore
      | CmdEcho _
      | CmdGetValue _
      | CmdMetaInfo _
      | CmdPop _
      | CmdPush _
      | CmdReset
      | CmdResetAssertions
      | CmdSetInfo _
      | CmdSetLogic _
      | CmdSetOption _
      | CmdCheckSatAssuming _ -> r
      | CmdDeclareConst (_, sort) -> check_sort r sort
      | CmdDeclareFun (_,  _, sorts_param,  sort) ->
         List.fold_left check_sort (check_sort r sort) sorts_param
      | CmdDefineFun fun_def -> check_fun_def r fun_def
      | CmdDefineFunRec fun_rec_defs ->
         List.fold_left check_fun_rec_def r fun_rec_defs
      | CmdDefineSort (_, _, sort) -> check_sort r sort
    ;;

    let check_script (s : Ast.script) =
        List.fold_left
          (fun r cmd -> check_command r cmd)
          { has_int = False; has_real = False; kind = None;}
          s.script_commands
    ;;

    let arithmetic s =
      match check_script s with
      | { has_int = True; has_real = True; kind = Some Difference; }
        -> Some Mixed, Some Linear (* RIDL does not exists. Upgrade it to LIRA *)
      | { has_int = True; has_real = True; kind } -> (Some Mixed), kind
      | { has_int = True; has_real = (Maybe | False) ; kind = Some _ as kind} ->
         (Some Integer), kind
      | { has_int = True; has_real = (Maybe | False) ; kind = None } ->
         Some Integer, Some Difference
      | { has_int = (Maybe | False); has_real = True; kind = Some _ as kind } ->
         (Some Real), kind
      | { has_int = (Maybe | False); has_real = True; kind = None } ->
         (Some Real), Some Difference

      | { has_int = False; has_real = False; _ } -> None, None
      | _ -> assert false
    ;;
end


let detect_logic (s : Ast.script) =
  let module Array = BasicInference(Theory.SMTArray) in
  let module BV = BasicInference(Theory.SMTBitVectors) in
  let arithmetic_sort, arithmetic_kind = ArithmeticCheck.arithmetic s in
  let uninterpreted_functions = UF.has_uninterpreted_functions s in
  let quantifiers = QF.has_quantifier s in
  let array = Array.use_theory s in
  let bitvectors = BV.use_theory s in
  { Logic.default
    with array; bitvectors;
         uninterpreted_functions; quantifiers;
         arithmetic_kind; arithmetic_sort ;
  }
;;

let detect_and_print (s : Ast.script) =
  let logic = detect_logic s in
  Io.debug "Detected logic : %a (set by script at \"%s\")@."
                pp_from_core logic
                (Ast_utils.get_logic s)
  ;
  logic
;;
