(*********************************************************************************)
(*  Copyright (c) 2015, INRIA, Universite de Nancy 2 and Universidade Federal    *)
(*  do Rio Grande do Norte.                                                      *)
(*                                                                               *)
(*  Permission to use, copy, modify, and distribute this software for any        *)
(*  purpose with or without fee is hereby granted, provided that the above       *)
(*  copyright notice and this permission notice appear in all copies.            *)
(*                                                                               *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES     *)
(*  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF             *)
(*  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR      *)
(*  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES       *)
(*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN        *)
(*  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF      *)
(*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.               *)
(*********************************************************************************)

open Ast
open Pp
open Pre_utils
open Ast_utils
;;

exception TermError of string;;

let nb_occurs_res = ref (Hashtbl.create 10000);;
let global_vars = ref [];;
let local_vars = ref [];;
let cc_list = ref [];;
let var_ordering = ref [];;
let index_var_ord = ref 0;;


(**********************************************************************)

(* replace let terms by real term *)
let rec pre_LA_replace_binds_term vbindings term =
  match vbindings, term.term_desc with
  | [], _ -> term
  | _ , TermLetTerm (vbindings_int, t) -> pre_LA_replace_binds_term vbindings (pre_LA_replace_binds_term vbindings_int t)
  | _ , TermSpecConstant _ -> term

  | (vb::vbs), TermQualIdentifier qid ->
    (match qid.qual_identifier_desc with
     | QualIdentifierIdentifier id -> 
       (match id.id_desc with
       | IdSymbol symbol ->
         let vbind_desc = vb.var_binding_desc in 
         (match vbind_desc with
          | VarBinding (symb, term_b) ->  (match symb.symbol_desc, symbol.symbol_desc with
                                           | SimpleSymbol s1, SimpleSymbol s2
                                           | QuotedSymbol s1, QuotedSymbol s2
                                           | SimpleSymbol s1, QuotedSymbol s2
                                           | QuotedSymbol s1, SimpleSymbol s2 ->
                                             if (s1 = s2) then term_b else pre_LA_replace_binds_term vbs term;)
                                          )
       | IdUnderscore (symbol, _) -> 
         let vbind_desc = vb.var_binding_desc in 
         (match vbind_desc with
          | VarBinding (symb, term_b) ->  (match symb.symbol_desc, symbol.symbol_desc with
                                           | SimpleSymbol s1, SimpleSymbol s2
                                           | QuotedSymbol s1, QuotedSymbol s2
                                           | SimpleSymbol s1, QuotedSymbol s2
                                           | QuotedSymbol s1, SimpleSymbol s2 ->
                                             if (s1 = s2) then term_b else pre_LA_replace_binds_term vbs term;)
                                          )
          )
     | QualIdentifierAs (_, _) -> term)
  
  | _ , TermQualIdentifierTerms (id, terms) -> { term_desc = TermQualIdentifierTerms (id, (List.map (fun t1 -> pre_LA_replace_binds_term vbindings t1) terms)); 
                                                 term_loc = term.term_loc;}
  | _, TermForallTerm (_, _) 
  | _, TermExistsTerm (_, _) 
  | _, TermAnnotatedTerm (_, _) -> term                                                 
;;

let pre_LA_subs_let_expr_term t =
  match t.term_desc with
  | TermLetTerm (vbindings, term) -> pre_LA_replace_binds_term vbindings term
  | TermSpecConstant _
  | TermQualIdentifier _
  | TermQualIdentifierTerms (_, _)
  | TermForallTerm (_, _)
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _) -> t
;;

let rec merge_monoms t1 t2 =
  (*        print_string "\n---------------merge t1\n";
        pp_pre_term Format.std_formatter t1;
        print_string "\n---------------merge t2\n";
        pp_pre_term Format.std_formatter t2;
        print_string "\n---------------merge t2\n";
  *)
  match (t1.term_desc, t2.term_desc) with
  | TermQualIdentifierTerms (qid1, ts1), TermQualIdentifier qid2  ->
    (
      { term_desc = (TermQualIdentifierTerms(qid1, List.append ts1 [t2])); 
        term_loc = t1.term_loc}
    )
  | TermQualIdentifierTerms (qid1, ts1), TermQualIdentifierTerms (qid2, ts2) ->
    (
      match ((pre_LA_get_term_operator t1), (pre_LA_get_term_operator t2)) with
      | "*", "*" -> 
        (
          { term_desc = (TermQualIdentifierTerms((mk_qual_id "*"  t1.term_loc), List.append ts1 ts2)); 
            term_loc = t1.term_loc};
        )
      | "*", "+" ->
      (
        let new_terms = List.map (fun x -> (merge_monoms t1 x)) ts2 in
        let res = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+"  t1.term_loc), new_terms)); 
                    term_loc = t1.term_loc} in

        res
      ) 
      | _, _ -> raise (TermError "ERROR: merge_monoms: combination not considered")
      raise (TermError "ERROR: merge_monoms: case not yet considered 2000")
    )
  | _ -> raise (TermError "ERROR: merge_monoms: case not yet considered 3000")

(* ZERO simplification step : replace (+ t1 (+ t2 t3) t4) by (+ t1 t2 t3 t4)  *)
let rec pre_flat_term t =
  match t.term_desc with
  | TermLetTerm (vbindings, term) -> raise (TermError "ERROR: pre_flat_term: let term not expected")
  | TermSpecConstant _
  | TermQualIdentifier _ -> t
  | TermForallTerm (_, _)
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _)  -> raise (TermError "ERROR: pre_flat_term: nor annotated terms nor quantified terms implemented")
  | TermQualIdentifierTerms (qual_id, terms) -> 
    let terms_simp = (List.map pre_flat_term terms) in
    if (pre_LA_is_arith_op t) then
    (
      if ( List.for_all  (fun x -> (is_identif x) || (is_coeff x)) terms_simp) then
         {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;}
      else
      (
        if (is_coeff t) then
          t
        else
        (
          let terms_monoms = ref (List.filter (fun x -> ((is_identif x) || (is_coeff x))) terms_simp) in
          let terms_no_monoms =  List.filter (fun x -> not ((is_identif x) || (is_coeff x))) terms_simp in
          let new_terms_no_monoms = List.map pre_flat_term terms_no_monoms in
          match (pre_LA_get_term_operator t) with
          | "+" ->
            (
              for i=0 to (List.length  new_terms_no_monoms) - 1 do
                let curr_term = (List.nth new_terms_no_monoms i) in
                if (pre_LA_get_term_operator curr_term = "+") then
                  terms_monoms := List.append !terms_monoms (pre_LA_get_terms_rel_expr curr_term)
                else
                  terms_monoms := List.append !terms_monoms [(pre_flat_term curr_term)]
              done;

	      { term_desc = (TermQualIdentifierTerms(qual_id, !terms_monoms)); 
                term_loc = t.term_loc;}
            )
          | "*" ->
            (
              for i=0 to (List.length  new_terms_no_monoms) - 1 do
                let curr_term = (List.nth new_terms_no_monoms i) in
                if (pre_LA_get_term_operator curr_term = "*") then
                  terms_monoms := List.append !terms_monoms (pre_LA_get_terms_rel_expr curr_term)
                else
                  terms_monoms := List.append !terms_monoms [(pre_flat_term curr_term)]
              done;

	      { term_desc = (TermQualIdentifierTerms(qual_id, !terms_monoms)); 
                term_loc = t.term_loc;}

            ) 
          | _ -> raise (TermError "ERROR: pre_flat_term: operator not yet implemented")
        );
      );
    )
    else
      if (pre_LA_is_logic_oper t) then
      (

        let op = (pre_LA_get_term_operator t) in
        let new_terms = ref [] in
        for i=0 to ( List.length  terms_simp) - 1 do
          let curr_term = (List.nth terms_simp i) in
          if (pre_LA_get_term_operator curr_term = op) then
            new_terms := List.append !new_terms (pre_LA_get_terms_rel_expr curr_term)
          else
            new_terms := List.append !new_terms [(pre_flat_term curr_term)]
        done;

	{ term_desc = (TermQualIdentifierTerms(qual_id, !new_terms)); 
          term_loc = t.term_loc;} 
      )
      else
        (
          {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;};
        );      
;;

(* term_distribution: given (\* a (+ b c)) returns (+ (\* a b) (\* a c)) *)
let rec pre_term_distribution t =
  match t.term_desc with
  | TermLetTerm (vbindings, term) -> raise (TermError "ERROR: pre_flat_term: let term not expected")
  | TermSpecConstant _
  | TermQualIdentifier _ -> t
  | TermForallTerm (_, _)
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _)  -> raise (TermError "ERROR: pre_flat_term: nor annotated terms nor quantified terms implemented")
  | TermQualIdentifierTerms (qual_id, terms) -> 
    let terms_simp = (List.map pre_term_distribution terms) in
    if  (pre_LA_is_arith_op t) then
    (
      match (pre_LA_get_term_operator t) with
      | "*" ->
      (
        let simple_sub_terms =  List.filter (fun x -> ((is_identif x) || (is_coeff x))) terms_simp in
        let complex_sub_terms =  List.filter (fun x -> not ((is_identif x) || (is_coeff x))) terms_simp in
        if ( List.length  complex_sub_terms == 0) then
          {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;}
        else
        (
          (*print_string ("\n--------------------\n");
          pp_pre_term Format.std_formatter t;
          print_string ("\n--------------------\n");
          *)
          let head_term = {term_desc = (TermQualIdentifierTerms(qual_id, simple_sub_terms)); term_loc = t.term_loc;} in
          let res = ref (merge_monoms head_term (List.hd complex_sub_terms)) in
  
          (*
          print_string ("\n--------------------\n");
          pp_pre_term Format.std_formatter head_term;
          print_string ("\n--------------------\n");
          *)

          for i=1 to ( List.length  complex_sub_terms) - 1 do
            res := (merge_monoms !res (List.hd complex_sub_terms))
          done;

          !res
        )
      )
      | "+" -> {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;}
      | "-" -> {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;}
      | _ -> 
        (
          print_string ("\n--------------------\n");
          pp_pre_term Format.std_formatter t;
          print_string ("\n--------------------\n");
          raise (TermError "ERROR: pre_term_distribution: operator not yet considered")
        )
    )
    else
      {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;};
;;

(* term_distribution: given (> a (+ b c)) returns (< (+ b c (\* (- 1) a)) 0 ) *)
let rec pre_term_clear_rhs t =
  match t.term_desc with
  | TermLetTerm (vbindings, term) -> raise (TermError "ERROR: pre_term_clear_rhs: let term not expected")
  | TermSpecConstant _
  | TermQualIdentifier _ -> t
  | TermForallTerm (_, _)
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _)  -> raise (TermError "ERROR: pre_term_clear_rhs: nor annotated terms nor quantified terms implemented")
  | TermQualIdentifierTerms (qual_id, terms) ->
                

    let terms_simp = (List.map pre_term_clear_rhs terms) in 
    if (pre_LA_is_rel_constraint(t)) then
    (

      let terms_len = List.length terms in
      if (terms_len = 2) then
      (
        let lhs = (List.nth terms 0) in
        let rhs = (List.nth terms 1) in
        match rhs.term_desc with
        | TermSpecConstant _ ->
        (
          if (pre_LA_get_numeral_const_expr rhs) = "0.0" || (pre_LA_get_numeral_const_expr rhs) = "0"  then
            t
          else
            match lhs.term_desc with 
            | TermQualIdentifier _ ->
              (
                let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), [lhs;(pre_LA_get_opposite_term rhs "1")])); 
                                term_loc = t.term_loc;} in
                let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
                { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); 
                  term_loc = t.term_loc;}
              )
            | TermQualIdentifierTerms (qid_lhs, lhs_ts) ->
              (
              match (pre_LA_get_term_operator lhs) with
              | "*" ->
                (
                  let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), [lhs;(mk_constant_term_num_neg (pre_LA_get_const_expr rhs) t.term_loc) ])); 
                                  term_loc = t.term_loc;} in
                  let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
                  { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); term_loc = t.term_loc;} 
                )
              | "+" ->
                (
                  let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), (List.append (pre_LA_get_terms_rel_expr lhs)  [(mk_constant_term_num_neg (pre_LA_get_const_expr rhs) t.term_loc) ]))); 
                                  term_loc = t.term_loc;} in
                  let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
                  { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); term_loc = t.term_loc;} 
                )
              | _ -> raise (TermError "ERROR: pre_term_clear_rhs: complete operator 500")
              )
            | _ -> 
              (
                print_string ("\n--------------------t\n");
                pp_pre_term Format.std_formatter t;

                print_string ("\n--------------------lhs\n");
                pp_pre_term Format.std_formatter lhs;
                print_string ("\n--------------------rhs\n");
                pp_pre_term Format.std_formatter rhs;
                print_string ("\n");

                raise (TermError "ERROR: pre_term_clear_rhs: case not yet considered in rhs constant 50")
              )
        )
        | TermQualIdentifierTerms (qid_rhs, rhs_ts) ->
        (
          match lhs.term_desc with
          | TermQualIdentifierTerms (qid_lhs, lhs_ts) ->
            ( 
            match  (pre_LA_get_term_operator lhs), (pre_LA_get_term_operator rhs) with
            | "*", "+" -> 
              (
                let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), (List.append (pre_LA_get_terms_rel_expr rhs) [(pre_LA_get_opposite_term lhs "1")]) )); 
                                term_loc = t.term_loc;} in
                let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
                match (pre_LA_get_term_operator t) with
                | ">=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "<=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                | "<=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id ">=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                | ">" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "<" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                | "<" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id ">" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                | "=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                | _ -> raise (TermError "ERROR: pre_term_clear_rhs: unpossible relational operator")

              )
            | "+", "+" -> 
              (
                let rhs_opposite = List.map (fun x -> (pre_LA_get_opposite_term x "1")) rhs_ts in
                let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), (List.append  (pre_LA_get_terms_rel_expr lhs) rhs_opposite))); 
                                term_loc = t.term_loc;} in
                let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
                { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); 
                  term_loc = t.term_loc;} 
              )

            | "*", "*" -> 
              (
                let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "-" t.term_loc), [lhs;rhs])); 
                                term_loc = t.term_loc;} in
                let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
                { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); term_loc = t.term_loc; }
               )
            | "+", "*" -> 
              (
                let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), (List.append  (pre_LA_get_terms_rel_expr lhs) [(pre_LA_get_opposite_term rhs "1")]) )); 
                                term_loc = t.term_loc;} in
                let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
                { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); term_loc = t.term_loc;} 
              )
            | "*", "-" -> 
              (
                if (is_coeff rhs) then
                (
                  let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), [lhs; (pre_LA_get_opposite_term rhs "1")]) ); 
                                  term_loc = t.term_loc;} in
                  let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
                  { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); term_loc = t.term_loc;}  
                )
                else raise (TermError "ERROR: pre_term_clear_rhs: complete negative case!")
              )
            | _, _ -> 
              (
                print_string ("\n--------------------t\n");
                pp_pre_term Format.std_formatter t;

                print_string ("\n--------------------lhs\n");
                pp_pre_term Format.std_formatter lhs;
                print_string ("\n--------------------rhs\n");
                pp_pre_term Format.std_formatter rhs;
                print_string ("\n--------------------OPPOSITE lhs\n");
                pp_pre_term Format.std_formatter (pre_LA_get_opposite_term lhs "1");

    

                raise (TermError "ERROR: pre_term_clear_rhs: operatorns not yet considered in rhs TermQualIdentifierTerms")
              )
            )
          | TermQualIdentifier _ ->
            (
              let new_rhs = (mk_numeral_term "0.0" t.term_loc) in              
              match (pre_LA_get_term_operator rhs) with
              | "+" ->
                (
                  let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), (List.append  rhs_ts [(pre_LA_get_opposite_term lhs "1")]))); 
                                  term_loc = t.term_loc;}  in
                  match (pre_LA_get_term_operator t) with
                  | ">=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "<=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | "<=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id ">=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | ">" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "<" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | "<" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id ">" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | "=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | _ -> raise (TermError "ERROR: pre_term_clear_rhs: unpossible relational operator")
                )
              | "*" ->
                (
                  let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "-" t.term_loc), [rhs; lhs])); term_loc = t.term_loc;} in
                  match (pre_LA_get_term_operator t) with
                  | ">=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "<=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | "<=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id ">=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | ">" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "<" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | "<" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id ">" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | "=" -> { term_desc = (TermQualIdentifierTerms((mk_qual_id "=" t.term_loc), [new_lhs; new_rhs])); term_loc = t.term_loc;}
                  | _ -> raise (TermError "ERROR: pre_term_clear_rhs: unpossible relational operator")

                )
              | _ -> raise (TermError "ERROR: pre_term_clear_rhs: complete operator");
             
              
            )
          | _ -> 
            (
              print_string ("\n--------------------t100\n");
              pp_pre_term Format.std_formatter t;

              raise (TermError "ERROR: pre_term_clear_rhs: case not yet considered in rhs TermQualIdentifierTerms")
            )
        )
        | TermQualIdentifier _ ->
        (
          match lhs.term_desc with
          | TermQualIdentifierTerms (qid_lhs, lhs_ts) ->
          ( 
            match (pre_LA_get_term_operator lhs) with
            | "+" ->
            (
              let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "+" t.term_loc), (List.append  (pre_LA_get_terms_rel_expr lhs) [(pre_LA_get_opposite_term rhs "1")]) )); 
                              term_loc = t.term_loc;} in
              let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
              { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); 
                term_loc = t.term_loc;}
            )
            | "*" ->
            (
              let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "-" t.term_loc), [lhs;rhs])); 
                              term_loc = t.term_loc;} in
              let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
              { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); 
                term_loc = t.term_loc;} 
            )
            | _ -> 
              (
                print_string ("\n--------------------term\n");
                pp_pre_term Format.std_formatter t;
                print_string ("\n--------------------lhs\n");
                pp_pre_term Format.std_formatter lhs;
                print_string ("\n--------------------rhs\n");
                pp_pre_term Format.std_formatter rhs;

                raise (TermError "ERROR: pre_term_clear_rhs: operatorns not yet considered in rhs TermQualIdentifier 100") 
              )
          )
          | TermQualIdentifier _ ->
            (
              let new_lhs = { term_desc = (TermQualIdentifierTerms((mk_qual_id "-" t.term_loc), [lhs;rhs])); 
                              term_loc = t.term_loc;} in
              let new_rhs = (mk_numeral_term "0.0" t.term_loc) in
              { term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t), [new_lhs; new_rhs])); 
                term_loc = t.term_loc;}
            )
          | _ ->  
            (
              print_string ("\n--------------------term\n");
              pp_pre_term Format.std_formatter t;
              print_string ("\n--------------------lhs\n");
              pp_pre_term Format.std_formatter lhs;
              print_string ("\n--------------------rhs\n");
              pp_pre_term Format.std_formatter rhs;
              raise (TermError "ERROR: pre_term_clear_rhs: operatorns not yet considered in rhs TermQualIdentifier 100500")
            )
        )
        | _ -> 
        (
          print_string ("\n--------------------term\n");
          pp_pre_term Format.std_formatter t;
          print_string ("\n--------------------lhs\n");
          pp_pre_term Format.std_formatter lhs;
          print_string ("\n--------------------rhs\n");
          pp_pre_term Format.std_formatter rhs;

          print_string ("\n--------------------\n");

           raise (TermError "ERROR: pre_term_clear_rhs: case not yet considered")
        )       
      )
      else
        raise (TermError "ERROR: pre_term_clear_rhs: relational operator must have only two sub terms")
    )
    else
      {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;};
;;

(* FIRST simplification step : logical and arithmetic trivial simplification *)
let rec pre_LA_trivial_simp_term t =
  match t.term_desc with
  | TermLetTerm (var_binds, tl) -> 
    (
      (*
      let new_binds = map
                    (fun vb ->
                      match vb.var_binding_desc with
                      | VarBinding (symbol, term) -> 
                        (
                          (match symbol.symbol_desc with
                           | SimpleSymbol s
                           | QuotedSymbol s -> 
                            
                               print_string ("\n" ^ s ^ "\n");
                               pp_pre_term Format.std_formatter term;
                            
                          );
                          { var_binding_desc = VarBinding (symbol, (pre_LA_trivial_simp_term term));
                                                           var_binding_loc = vb.var_binding_loc }
                        )
                    ) var_binds in
      print_string ("\n\n");
      pp_pre_term Format.std_formatter tl;
      {term_desc = TermLetTerm (new_binds, (pre_LA_trivial_simp_term tl)); term_loc = t.term_loc;};
       *)
       raise (TermError "No let command expected") 
    )
  | TermSpecConstant _
  | TermQualIdentifier _
  | TermForallTerm (_, _)
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _) -> t
  | TermQualIdentifierTerms (qual_id, terms) -> 
    let qid = id_of_qid qual_id in
    if (pre_LA_is_rel_constraint(t)) then
       let terms_len = List.length  terms in
       if (terms_len = 2) then
         (
         let lhs = (List.nth terms 0) in
         let rhs = (List.nth terms 1) in
         let lhs_simp = (pre_LA_trivial_simp_term lhs) in
         let rhs_simp = (pre_LA_trivial_simp_term rhs) in
         if (pre_LA_is_numeral_constant_term(lhs_simp) && pre_LA_is_numeral_constant_term(rhs_simp)) then
           (
            (*
            print_string "\n-------RELAT INIT \n";
            pp_pre_term Format.std_formatter lhs_simp;
            print_string "\n-- \n";
            pp_pre_term Format.std_formatter rhs_simp;
            print_string "\n-------RELAT END \n";
            *)
            let lhs_num = float_of_string (pre_LA_get_numeral_const_expr lhs_simp) in
            let rhs_num = float_of_string (pre_LA_get_numeral_const_expr rhs_simp) in
            match (pre_LA_get_qual_id_expr qid) with
            |"=" -> {term_desc = (TermSpecConstant(CstBool (lhs_num = rhs_num))); term_loc = t.term_loc;}
            |">" -> {term_desc = (TermSpecConstant(CstBool (lhs_num > rhs_num))); term_loc = t.term_loc;}
            |">=" -> {term_desc = (TermSpecConstant(CstBool (lhs_num >= rhs_num))); term_loc = t.term_loc;}
            |"<" -> {term_desc = (TermSpecConstant(CstBool (lhs_num < rhs_num))); term_loc = t.term_loc;}
            |"<=" -> {term_desc = (TermSpecConstant(CstBool (lhs_num <= rhs_num))); term_loc = t.term_loc;}
            | op -> 
              (
                print_string op;
                print_string "\n";
                raise (TermError "ERROR : it is not a relatinal term")
              ) 
           )
         else 
           {term_desc = (TermQualIdentifierTerms(qual_id, [lhs_simp; rhs_simp])); term_loc = t.term_loc;};
         )
       else raise (TermError "Rel operator  List.length  error") 
    else 
      if (pre_LA_is_logic_oper(t)) then
      ( let terms_simp = (List.map pre_LA_trivial_simp_term terms) in
        match (pre_LA_get_term_operator t) with
        | "and" -> 
        (
          if ((List.length  (List.filter (fun x-> (pre_LA_is_boolean_constant_term x) && not (pre_LA_get_boolean_const_expr x) ) terms_simp)) > 0) then 
            {term_desc = (TermSpecConstant(CstBool false)); term_loc = t.term_loc;}
          else
            (
            let terms_simp_filtered = (List.filter (fun x -> not (pre_LA_is_boolean_constant_term x)) terms_simp) in
            match (List.length  terms_simp_filtered) with 
            | 0 -> {term_desc = (TermSpecConstant(CstBool true)); term_loc = t.term_loc;}
            | 1 -> {term_desc = (List.hd terms_simp_filtered).term_desc; term_loc = t.term_loc;}
            | _ -> {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp_filtered)); 
                    term_loc = t.term_loc;};
            )
          )
        | "or" -> 
        (

          if ((List.length  (List.filter (fun x-> (pre_LA_is_boolean_constant_term x) && (pre_LA_get_boolean_const_expr x) ) terms_simp)) > 0) then 
            {term_desc = (TermSpecConstant(CstBool true)); term_loc = t.term_loc;}
          else
            (
            let terms_simp_filtered = (List.filter (fun x -> not (pre_LA_is_boolean_constant_term x)) terms_simp) in
            match (List.length  terms_simp_filtered) with 
            | 0 -> {term_desc = (TermSpecConstant(CstBool false)); term_loc = t.term_loc;}
            | 1 -> {term_desc = (List.hd terms_simp_filtered).term_desc; term_loc = t.term_loc;}
            | _ -> {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp_filtered)); term_loc = t.term_loc;};
            )

          )      
        | "not" -> 
          (
            if (pre_LA_is_rel_constraint((List.nth terms_simp 0))) then
            (
              match  (pre_LA_get_term_operator (List.nth terms_simp 0)) with
              | "=" -> {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;}
              | "<=" -> {term_desc = (TermQualIdentifierTerms((mk_qual_id ">" t.term_loc), (pre_LA_get_terms_rel_expr (List.nth terms_simp 0)))); term_loc = t.term_loc;} 
              | "<" -> {term_desc = (TermQualIdentifierTerms((mk_qual_id ">=" t.term_loc), (pre_LA_get_terms_rel_expr (List.nth terms_simp 0)))); term_loc = t.term_loc;} 
              | ">=" -> {term_desc = (TermQualIdentifierTerms((mk_qual_id "<" t.term_loc), (pre_LA_get_terms_rel_expr (List.nth terms_simp 0)))); term_loc = t.term_loc;} 
              | ">" -> {term_desc = (TermQualIdentifierTerms((mk_qual_id "<=" t.term_loc), (pre_LA_get_terms_rel_expr (List.nth terms_simp 0)))); term_loc = t.term_loc;} 
              | _ ->
                (
                  {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;};
                  print_string "\n-------RELAT INIT \n";
                  pp_pre_term Format.std_formatter (List.nth terms_simp 0);
                  print_string "\n-- \n";
                  raise (TermError "NOT")
                )
            )
            else
            (
              {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;}
            )
          )
        | _ -> raise (TermError "Logical operator not yet implemented")
      )
      else
        if (pre_LA_is_arith_op(t)) then
          let terms_simp = (List.map pre_LA_trivial_simp_term terms) in
          match (pre_LA_get_term_operator t) with
          | "+" -> 
            (
             let list_zero = (List.filter (fun x-> ((pre_LA_is_numeral_constant_term x) && 
								        ((float_of_string (pre_LA_get_numeral_const_expr x)) = 0.0)) ) terms_simp) in
             if (List.length  list_zero >= 0) then
	      let terms_simp_filtered = (List.filter (fun x -> not (pre_LA_is_numeral_constant_term x) || not ((float_of_string (pre_LA_get_numeral_const_expr x)) = 0.0)) terms_simp) in
               match (List.length  terms_simp_filtered) with 
               | 0 -> {term_desc = (TermSpecConstant(CstNumeral "0")); term_loc = t.term_loc;}
	       | 1 -> {term_desc = (List.hd terms_simp_filtered).term_desc; term_loc = t.term_loc;}
               | _ ->
                 (
                   if ( List.for_all  (fun x -> (pre_LA_is_numeral_constant_term x) && 
                                         ((pre_LA_get_numeral_const_expr x) = "1.0" || (pre_LA_get_numeral_const_expr x) = "0.0")) terms_simp_filtered) then
                     ( 
                       mk_numeral_term ((string_of_float (float_of_int ( List.length  ( List.filter (fun x -> (pre_LA_get_numeral_const_expr x) = "1.0") terms_simp_filtered)))^"0"))  t.term_loc
                     )
                   else {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp_filtered)); term_loc = t.term_loc;};
                 )
	     else
               {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;};
            )
          | "*" -> 
            (
             if ((List.length  (List.filter (fun x-> (pre_LA_is_numeral_constant_term x) && 
								        ((float_of_string (pre_LA_get_numeral_const_expr x)) = 1.0) ) terms_simp)) > 0) then
	      let terms_simp_filtered = (List.filter (fun x -> not (pre_LA_is_numeral_constant_term x) || not ((float_of_string (pre_LA_get_numeral_const_expr x)) = 1.0)) terms_simp) in

               match (List.length  terms_simp_filtered) with 
               | 0 -> {term_desc = (TermSpecConstant(CstNumeral "1")); term_loc = t.term_loc;}
	       | 1 -> {term_desc = (List.hd terms_simp_filtered).term_desc; term_loc = t.term_loc;}
               | _ -> {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp_filtered)); term_loc = t.term_loc;};
             else
               if ((List.length  (List.filter (fun x-> (pre_LA_is_numeral_constant_term x) && 
								        ((float_of_string (pre_LA_get_numeral_const_expr x)) = 0.0) ) terms_simp)) > 0) then
	         {term_desc = (TermSpecConstant(CstNumeral "0")); term_loc = t.term_loc;}
	       else
                 {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;};
            )
          | _ -> {term_desc = (TermQualIdentifierTerms(qual_id, terms_simp)); term_loc = t.term_loc;}
           
         
        else
          {term_desc = (TermQualIdentifierTerms(qual_id, (List.map pre_LA_trivial_simp_term terms))); term_loc = t.term_loc;}
      ;
;;

let compare_length t1 t2 = match t1.term_desc, t2.term_desc with
  | TermQualIdentifierTerms (_, ts1), TermQualIdentifierTerms (_, ts2) -> 
  (
    let lhs1 = (List.hd ts1) in
    let lhs2 = (List.hd ts2) in
    match lhs1.term_desc, lhs2.term_desc with
    | TermQualIdentifierTerms (_, ts_lhs1), TermQualIdentifierTerms (_, ts_lhs2) -> 
      if ( List.length  ts_lhs1) < ( List.length  ts_lhs2) then -1
      else 
        if ( List.length  ts_lhs1) > ( List.length  ts_lhs2) then 1 
      else 0;
    | TermQualIdentifier _, TermQualIdentifierTerms (_, _) ->  -1
    | TermQualIdentifierTerms (_, _), TermQualIdentifier _ ->  1
    | TermQualIdentifier _, TermQualIdentifier _ ->  0
    | _, _ -> 
      (
        print_string "\n~~~lhs1\n";
        (pp_pre_term Format.std_formatter lhs1);
        print_string "\n~~~lhs2\n";
        (pp_pre_term Format.std_formatter lhs2);

        raise (TermError "ERROR: compare_length: case not considered 100")
      )
  )
  | _, _ -> raise (TermError "ERROR: compare_length: case not considered 200")
;;

let rec pre_LA_sort_terms t = match t.term_desc with
  | TermLetTerm (_, _) -> raise (TermError "No let command expected")
  | TermSpecConstant _
  | TermQualIdentifier _
  | TermForallTerm (_, _)
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _) -> t
  | TermQualIdentifierTerms (qual_id, terms) -> 
    if (pre_LA_is_arith_op(t)) then
    (
      match (pre_LA_get_term_operator t) with
      | "*" ->
        (
          let terms_coeff = ( List.filter is_coeff terms) in
          let terms_no_coeff = ( List.filter (fun x -> not (is_coeff x)) terms) in
          { term_desc = (TermQualIdentifierTerms(qual_id, (List.append  terms_no_coeff terms_coeff))); 
            term_loc = t.term_loc;};
        )
      | "+" -> {term_desc = (TermQualIdentifierTerms(qual_id, (List.sort compare_terms (List.map  pre_LA_sort_terms terms)))); term_loc = t.term_loc;}
      | _ -> t
    )
    else
      match (pre_LA_get_term_operator t) with
      | "and" ->
        (
          if ( List.for_all  pre_LA_is_rel_constraint terms) then
          (
            {term_desc = (TermQualIdentifierTerms(qual_id, (List.sort compare_length  terms))); term_loc = t.term_loc;}
          )
          else
            {term_desc = (TermQualIdentifierTerms(qual_id, (List.map  pre_LA_sort_terms terms))); term_loc = t.term_loc;};
        )
      | _ -> {term_desc = (TermQualIdentifierTerms(qual_id, (List.map  pre_LA_sort_terms terms))); term_loc = t.term_loc;}
;;

(* Secons simplification step : identify instersection on constraints *)
let rec pre_LA_trivial_rel_merge t =
  (*
  print_string "\n~~~\n";
  (pp_pre_term Format.std_formatter t);
  *)
  match t.term_desc with
  | TermLetTerm (var_bindings, term) -> raise (TermError "LET error")
  | TermSpecConstant _
  | TermQualIdentifier _
  | TermForallTerm (_, _)
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _) -> t
  | TermQualIdentifierTerms (qual_id, terms) -> 
    if (pre_LA_is_logic_oper(t)) then
      let terms_simp = (List.map pre_LA_trivial_rel_merge terms) in
      match (pre_LA_get_term_operator t) with
      | "and" ->
        (
         let rel_terms = (List.filter (fun x -> (pre_LA_is_rel_constraint x) && 
						     (((pre_LA_get_rel_expr x) = "<=") || ((pre_LA_get_rel_expr x) = "<"))) terms) in
	 match (List.length  rel_terms) with
         | 2 -> 
           (
            let t1 = (List.nth rel_terms 0) in
            let op1 = (pre_LA_get_rel_expr t1) in
            let terms1 = (pre_LA_get_terms_rel_expr t1) in
            let lhs1 = (List.nth terms1 0) in
            let rhs1 = (List.nth terms1 1) in
            let t2 = (List.nth rel_terms 1) in
            let op2 = (pre_LA_get_rel_expr t2) in
            let terms2 = (pre_LA_get_terms_rel_expr t1) in
            let lhs2 = (List.nth terms2 0) in
            let rhs2 = (List.nth terms2 1) in
            match op1, op2 with
            | "=", "="
            | "=", "<="
            | "=", ">="
            | "=", "<"
            | "=", ">" 
            | "<=", ">="
            | "<=", ">"
            | ">=", "<"
            | ">=", ">"
            | "<", ">"->
              (
                print_string ("op1: " ^ op1 ^ " op2: " ^ op2);
                print_string "\n~~~T1\n";
                (pp_pre_term Format.std_formatter t1);
                print_string "\n~~~T2\n";
                (pp_pre_term Format.std_formatter t2);

                raise (TermError "ERROR: pre_LA_trivial_rel_merge combination not yet considered")
              )
            | "=", "="
            | "<=", "<="
            | ">=", ">="
            | "<", "<"
            | ">", ">" ->
              (
 	        if (rhs1 = rhs2 && lhs1 = lhs2) then (pre_LA_trivial_rel_merge t1)
                else 
                  {term_desc = (TermQualIdentifierTerms(qual_id, (List.map pre_LA_trivial_rel_merge terms_simp))); term_loc = t.term_loc;};
              )
            | "<=", "<" ->
	        if (rhs1 = rhs2 && lhs1 = lhs2) then 
                  (
                   let comp_terms = (List.filter (fun x -> not (pre_LA_is_rel_constraint x) || 
						     (not ((pre_LA_get_rel_expr x) = "<=") && not ((pre_LA_get_rel_expr x) = "<"))) terms) in
                   {term_desc = (TermQualIdentifierTerms(qual_id, (List.append comp_terms [{term_desc = (TermQualIdentifierTerms((pre_LA_get_qual_identifier t2), [lhs1; rhs1])); 
                                                                                            term_loc = t.term_loc;}])));
                    term_loc = t.term_loc;};
                  )
                else 
                  {term_desc = (TermQualIdentifierTerms(qual_id, (List.map pre_LA_trivial_rel_merge terms_simp))); term_loc = t.term_loc;};
            | _ -> {term_desc = (TermQualIdentifierTerms(qual_id, (List.map pre_LA_trivial_rel_merge terms_simp))); term_loc = t.term_loc;};
            
           )
         | _ -> {term_desc = (TermQualIdentifierTerms(qual_id, (List.map pre_LA_trivial_rel_merge terms_simp))); term_loc = t.term_loc;};
        )
      | _ -> {term_desc = (TermQualIdentifierTerms(qual_id, (List.map pre_LA_trivial_rel_merge terms_simp))); term_loc = t.term_loc;};
    else t;
;;

(* given term (monoms) it returns if it's a global or local one (only linear case) *)
let is_global_sub_term t =
  match t.term_desc with
  | TermSpecConstant _ -> false
  | TermQualIdentifier qual_id ->
    (match qual_id.qual_identifier_desc with
     | QualIdentifierIdentifier id ->
       (
       match id.id_desc with
       | IdSymbol symbol 
       | IdUnderscore (symbol, _) ->          
         (match symbol.symbol_desc with
          | SimpleSymbol s
          | QuotedSymbol s -> (List.mem s !global_vars)
         )
       )
     | _ -> raise (TermError "ERROR: is_global_sub_term not yet considered")
    )
  | TermQualIdentifierTerms (_, terms) -> 
    (
     match (pre_LA_get_term_operator t) with
     | "*" ->
       ( 
        match ( List.length  terms) with
        | 2 -> 
          (match (List.nth terms 0).term_desc, (List.nth terms 1).term_desc with
          | TermQualIdentifier id, TermSpecConstant _ 
          | TermSpecConstant _, TermQualIdentifier id 
          | TermQualIdentifier id, TermQualIdentifierTerms (_, _) 
          | TermQualIdentifierTerms (_, _), TermQualIdentifier id ->
            (match id.qual_identifier_desc with
             | QualIdentifierIdentifier id ->
               (match id.id_desc with
                | IdSymbol symbol 
                | IdUnderscore (symbol, _) ->          
                  (match symbol.symbol_desc with
                   | SimpleSymbol s
                   | QuotedSymbol s -> (List.mem s !global_vars)
                  )
                )
             | _ -> raise (TermError "ERROR: is_global_sub_term not yet considered 100")
            )
          | _, _ -> raise (TermError "ERROR: is_global_sub_term not yet considered 200")
          )
        | _ -> raise (TermError "More than two operands not yet implemented 300")
       )
     | _ -> 
       (
         print_string (pre_LA_get_term_operator t);
         raise (TermError "No complex subterms expected")
       )
    )
  | _ -> raise (TermError "Only constant and qualified identifier expected")
;;


let pre_LA_split_add_term t =
  let op = (pre_LA_get_term_operator t) in
  let terms = (pre_LA_get_terms_rel_expr t) in
  match ( List.length  terms) with
  |2 -> 
    (
      if (is_identif_or_mult (List.nth terms 0)) && (is_identif_or_mult (List.nth terms 1)) then
      (
        match (List.nth terms 0).term_desc, (List.nth terms 1).term_desc with
        | TermQualIdentifier _, _ -> 
          (
            if (not (List.exists (fun x -> (are_equiv t (fst x)) || (are_equiv (fst x) t)) !cc_list)) then
              cc_list := List.append !cc_list [(t, ([(List.nth terms 0)], [(pre_LA_get_opposite_term (List.nth terms 1) "1")]))];
          )
        | _, TermQualIdentifier _ ->  
          (
            if (not (List.exists   (fun x -> (are_equiv t (fst x)) || (are_equiv (fst x) t)) !cc_list)) then
              cc_list := List.append !cc_list [(t, ([(List.nth terms 1)], [(pre_LA_get_opposite_term (List.nth terms 0) "1")]))];
          )
        | TermQualIdentifier _, TermSpecConstant _ -> raise (TermError "Constant")
	| TermQualIdentifierTerms (_, tsl), TermQualIdentifierTerms (_, tsr) ->
          (
            if ( List.length  tsl) = ( List.length  tsr) && ( List.length  tsl) = 2 then
            (
              match (pre_LA_get_term_operator (List.nth terms 0)), (pre_LA_get_term_operator (List.nth terms 1)) with
              | "*", "*" ->
                (
		  let tsl_lt = (List.nth tsl 0) in
		  let tsl_rt = (List.nth tsl 1) in
		  let tsr_lt = (List.nth tsr 0) in
		  let tsr_rt = (List.nth tsr 1) in
		  match tsl_lt.term_desc, tsl_rt.term_desc, tsr_lt.term_desc, tsr_rt.term_desc with
                  | TermQualIdentifier _, TermQualIdentifierTerms (_, terms_tsl_rt), TermQualIdentifier _, TermSpecConstant _ ->
                    cc_list := List.append !cc_list [(t, ([(List.nth terms 1)], [(pre_LA_get_opposite_term (List.nth terms 0) "1")]))]  
                  | _, _, _, _ -> ()
                  (* raise (TermError "Split case: not implemented yet")  *)
                )
              | _, _ -> ()
            )
          )
        | _, _ -> 
          ()
      )
    )
  | _ ->
    (
      match ( List.for_all  (fun x -> (is_identif_or_mult x)) terms) with
      | true ->
        (
          let global_sub_terms = List.map (fun y -> (pre_LA_get_opposite_term y "1")) ( List.filter (fun x -> (is_global_sub_term x)) terms) in
          let local_sub_terms =  List.filter (fun x -> (not (is_global_sub_term x))) terms in
          match ( List.length  global_sub_terms) + ( List.length  local_sub_terms) = ( List.length  terms) with
          | true ->
            (
              match op = "+" && ( List.length  global_sub_terms) > 0 && ( List.length  local_sub_terms) > 0 with
              | true -> 
                (
                  
                  if ( List.length  local_sub_terms) >= ( List.length  global_sub_terms) then
                  (
                    if (not (List.exists   (fun x -> (are_equiv t (fst x)) || (are_equiv (fst x) t)) !cc_list)) then
                    (
                      cc_list := List.append !cc_list [(t, (local_sub_terms, global_sub_terms))];
                    )
                  );
                  ()
                )
              | false -> ()
            )
          | false -> raise (TermError "pre_LA_split_add_term: global and local do not have same  List.length  than all terms")
        )
      | false -> ()
    )
;;

let pre_LA_var_identify_replacing_pair t =
  (*
  print_string ("\n------------------Identify..." ^ (string_of_int ( List.length  !cc_list)) ^ "\n");
  (pp_pre_term Format.std_formatter t);  
  *)

  match (pre_LA_is_term t "=") with
  | true -> 
    let terms = (pre_LA_get_terms_rel_expr t) in
    if ( List.length  terms = 2) then
      (
        let lhs = (List.nth terms 0) in
        let rhs = (List.nth terms 1) in
        if ((pre_LA_is_numeral_constant_term rhs) && ((float_of_string (pre_LA_get_numeral_const_expr rhs)) = 0.0)) then
          (
            match (pre_LA_get_term_operator lhs) with
            | "+" -> 
              (
                (pre_LA_split_add_term lhs)
              )
            | "*" ->
              (
               let lhs_ts = (pre_LA_get_terms_rel_expr lhs) in
               match ( List.length  lhs_ts) with
               | 2 ->
                 (
                   let lhs_t1 = (List.nth lhs_ts 0) in
                   let lhs_t2 = (List.nth lhs_ts 1) in
                   match lhs_t1.term_desc, lhs_t2.term_desc with
                   | TermQualIdentifier _, _ -> 
                     (
                       cc_list := List.append !cc_list [(t, ([lhs_t1], [rhs]))];
                     )
                   | TermSpecConstant _, TermQualIdentifier _
                   | TermQualIdentifierTerms (_, _), TermQualIdentifier _ -> 
                     (
                       cc_list := List.append !cc_list [(t, ([lhs_t2], [rhs]))];
                     )

                   | _ -> 
                     (

                       print_string "\n------------------replace...\n";
                       (pp_pre_term Format.std_formatter t);  


                       raise (TermError "Error identify_replacing_pair: comb not yet implemented")
                     )
                 )
               | _ -> raise (TermError "Error identify_replacing_pair nb of terms error")
              )
              
 

            | _ -> ()
          )
        else
          (
            ()
          )
        
      )
    else
      print_string "Warning: equality term has more then 2 terms";
  | false -> raise (TermError "Non equality term received")
;;


(* in case of need add the negative of each list in a the list of terms*)
let pre_LA_new_lhs_terms t n1 n2 lhs_ts replacing_terms =
  let neg_replacing_terms = List.map (fun x -> (pre_LA_get_opposite_term x n1)) replacing_terms in
  (
    let lhs_terms = ref ( List.filter (fun x -> not (are_equiv x (pre_LA_get_opposite_term t n2))) !lhs_ts) in
        
    for i=1 to ( List.length  neg_replacing_terms) do
      if ( List.for_all  (fun x -> not (are_equiv (pre_LA_get_opposite_term (List.nth neg_replacing_terms (i-1)) "1") x)) !lhs_terms) then
        (
          let addable_terms = ( List.filter (fun x -> (are_addable x (List.nth neg_replacing_terms (i-1)))) !lhs_terms) in
          match ( List.length  addable_terms) with
          | 1 ->
            (       
              let not_addable_terms =  List.filter (fun x -> not (are_addable x (List.nth neg_replacing_terms (i-1)))) !lhs_terms in
              let add_term = (pre_LA_sum_up_mon (List.hd addable_terms) (List.nth neg_replacing_terms (i-1))) in
              (
                if ((pre_LA_is_numeral_constant_term add_term) && ((float_of_string (pre_LA_get_numeral_const_expr add_term)) = 0.0)) then
                  lhs_terms := not_addable_terms
                else
                  lhs_terms := add_term :: not_addable_terms;
              )
            )
          | _ ->
            (
              lhs_terms := (List.nth neg_replacing_terms (i-1)) :: !lhs_terms
            );                          
        )
      else
        (
          let terms_eqv = ( List.filter (fun x -> (are_equiv (pre_LA_get_opposite_term (List.nth neg_replacing_terms (i-1)) "1") x)) !lhs_terms) in
          match ( List.length  terms_eqv) with 
          | 1 -> lhs_terms := ( List.filter (fun x -> not (are_equiv (pre_LA_get_opposite_term (List.nth neg_replacing_terms (i-1)) "1") x)) !lhs_terms)
          | _ -> 
            (
              let addable_terms = ( List.filter (fun x -> (are_addable x (List.nth neg_replacing_terms (i-1)))) !lhs_terms) in
              match ( List.length  addable_terms) with
              | 1 ->
                (       
                  print_string "\n==============================================2000\n";          
                  let not_addable_terms =  List.filter (fun x -> not (are_addable x (List.nth neg_replacing_terms (i-1)))) !lhs_terms in
                  lhs_terms := (pre_LA_sum_up_mon (List.hd addable_terms) (List.nth neg_replacing_terms (i-1))) :: not_addable_terms;
                )
              | _ ->
                (

                  print_string "\n==============================================200\n";          
                  print_string "\n------------------ADDED in\n";
                  List.map (fun x -> print_string "\n";(pp_pre_term Format.std_formatter x); print_string "\n") !lhs_terms;     

                  print_string "\n------------------to ADD\n";
                  (pp_pre_term Format.std_formatter (List.nth neg_replacing_terms (i-1)));  


                  lhs_terms := (List.nth neg_replacing_terms (i-1)) :: !lhs_terms
                );                          
            )
        )
    done;
    !lhs_terms          
                     
  )
;;












(* in case of need add the positive of each list in a the list of terms*)
let pre_LA_add_pos_monoms t n1 n2 lhs_ts replacing_terms =
  let lhs_terms_fil = ref ( List.filter (fun x -> not (are_equiv (pre_LA_get_times_term t n1) x)) !lhs_ts) in
  for i = 1 to ( List.length  replacing_terms) do
    let term_to_add = (pre_LA_get_times_term (List.nth replacing_terms (i-1)) n2) in
    let term_to_add_neg = (pre_LA_get_opposite_term (List.nth replacing_terms (i-1)) n2) in
    let already_in = ( List.filter (fun x -> (are_equiv x term_to_add)) !lhs_terms_fil) in
    let already_in_neg = ( List.filter (fun x -> (are_equiv x term_to_add_neg)) !lhs_terms_fil) in
    match ( List.length  already_in) with
    | 1 -> lhs_terms_fil := ( List.filter (fun x -> not (are_equiv x term_to_add)) !lhs_terms_fil)
    | _ -> 
      match ( List.length  already_in_neg) with
      | 1 -> lhs_terms_fil := ( List.filter (fun x -> not (are_equiv x term_to_add_neg)) !lhs_terms_fil)
      | _ -> 
        (
          lhs_terms_fil := term_to_add :: !lhs_terms_fil
        )
  done;
  !lhs_terms_fil
;;

(*
let rec range a b =
  if a > b then []
  else a :: range (a + 1) b
*)

let pre_LA_simp_by_CC t p =
                         
  let to_replac_term = (fst p) in
  let replacing_term = (snd p) in
  let qual_id_rel = (pre_LA_get_qual_identifier t) in
  let terms = (pre_LA_get_terms_rel_expr t) in

  (*
  print_string "\n---------------------Term\n";
  pp_pre_term Format.std_formatter t;

  print_string "\n--------------------- TO replace\n";
  iter (fun x -> print_string "\n";(pp_pre_term Format.std_formatter x); print_string "\n") to_replac_term;

  print_string "\n--------------------- replacing\n";
  iter (fun x -> print_string "\n";(pp_pre_term Format.std_formatter x); print_string "\n") replacing_term;
  print_string "\n--\n";
  *)			 

  match ( List.length  to_replac_term), ( List.length  replacing_term), (List.nth replacing_term 0).term_desc with
  | 1, 1, TermSpecConstant _ ->
    (
      if ( List.length  terms = 2) then
        (
          let lhs = (List.nth terms 0) in
          let rhs = (List.nth terms 1) in
          if ((pre_LA_get_numeral_const_expr (List.hd replacing_term)) = "0.0") then
            (
                let lhs_terms = ref (pre_LA_get_terms_rel_expr lhs) in
                let i = ref 1 in
                let break = ref false in
                while ((!i < 100) && (not !break)) do
                  let neg_i_1 = { term_desc = TermQualIdentifierTerms((mk_qual_id "*"  t.term_loc), [(List.hd to_replac_term); (mk_constant_term_num_neg (string_of_int !i) t.term_loc)]); 
                                  term_loc = t.term_loc;} in
                  let neg_i_2 = { term_desc = TermQualIdentifierTerms((mk_qual_id "*"  t.term_loc), [(mk_constant_term_num_neg (string_of_int !i) t.term_loc); (List.hd to_replac_term)]); 
                                  term_loc = t.term_loc;} in
                  let pos_i_1 = { term_desc = TermQualIdentifierTerms((mk_qual_id "*"  t.term_loc), [(List.hd to_replac_term); (mk_constant_term_num_pos (string_of_int !i) t.term_loc)]); 
                                  term_loc = t.term_loc;} in
                  let pos_i_2 = { term_desc = TermQualIdentifierTerms((mk_qual_id "*"  t.term_loc), [(mk_constant_term_num_pos (string_of_int !i) t.term_loc); (List.hd to_replac_term)]); 
                                  term_loc = t.term_loc;} in

                  if (List.exists   (fun x -> are_equiv x neg_i_1) (pre_LA_get_terms_rel_expr (List.nth terms 0))) then
                  (
                    lhs_terms := ( List.filter (fun x -> (not (are_equiv x neg_i_1))) (pre_LA_get_terms_rel_expr (List.nth terms 0))); 
                    break:= true
                  );

                  if (List.exists   (fun x -> are_equiv x neg_i_2) (pre_LA_get_terms_rel_expr (List.nth terms 0))) then
                  (
                    lhs_terms := ( List.filter (fun x -> (not (are_equiv x neg_i_2))) (pre_LA_get_terms_rel_expr (List.nth terms 0))); 
                    break:= true
                  );

                  if (List.exists   (fun x -> are_equiv x pos_i_1) (pre_LA_get_terms_rel_expr (List.nth terms 0))) then
                  (
                    lhs_terms := ( List.filter (fun x -> (not (are_equiv x pos_i_1))) (pre_LA_get_terms_rel_expr (List.nth terms 0))); 
                    break:= true
                  );

                  if (List.exists   (fun x -> are_equiv x pos_i_2) (pre_LA_get_terms_rel_expr (List.nth terms 0))) then
                  (
                    lhs_terms := ( List.filter (fun x -> (not (are_equiv x pos_i_2))) (pre_LA_get_terms_rel_expr (List.nth terms 0))); 
                    break:= true
                  );
                       
                  i := !i + 1  
                done;


              match ( List.length  !lhs_terms) with
              | 0 -> { term_desc = TermQualIdentifierTerms(qual_id_rel, [(mk_constant_term_num_pos "0.0" t.term_loc); rhs]); term_loc = t.term_loc;} 
              | 1 -> { term_desc = TermQualIdentifierTerms(qual_id_rel, [(List.hd !lhs_terms); rhs]); term_loc = t.term_loc;} 
              | _ ->
                ( 
                  let new_lhs = { term_desc = TermQualIdentifierTerms ((mk_qual_id "+" t.term_loc), !lhs_terms); term_loc = t.term_loc;} in
                  { term_desc = TermQualIdentifierTerms(qual_id_rel, [new_lhs; rhs]); term_loc = t.term_loc;}
                )

            )
          else t;
        )
      else t;
    )
  | _, _, _ -> 
    (
      if ( List.length  terms = 2) then
        let lhs = (List.nth terms 0) in
        let rhs = (List.nth terms 1) in
        if ((pre_LA_is_numeral_constant_term rhs) && ((float_of_string (pre_LA_get_numeral_const_expr rhs)) = 0.0)) then
        (   
          match ( List.length  to_replac_term) with
          | 1 -> 
            (

             let t1 = (List.hd to_replac_term) in             
             match (is_sub_term t1 lhs) with
             | true ->
               (
                 let lhs_terms = (pre_LA_get_terms_rel_expr lhs) in
                 let new_lhs_ts = ref ( List.filter (fun x -> (not (are_equiv x t1))) lhs_terms) in
                 match ( List.length  ( List.filter (fun x -> (are_equiv x t1)) lhs_terms)) with
                 | 1 -> 

                   (
                     for i=( List.length  replacing_term) - 1 downto 0  do
                       let addable_terms = ( List.filter (fun x -> (are_addable x (List.nth replacing_term i))) !new_lhs_ts) in
                       let not_addable_terms =  List.filter (fun x -> not (are_addable x (List.nth replacing_term i))) !new_lhs_ts in
                       match ( List.length  addable_terms) with
                       | 1 ->
                         (
                           let added_term = (pre_LA_sum_up_mon (List.hd addable_terms) (List.nth replacing_term i)) in
                           if not (are_equiv (mk_constant_term_num_pos "0" t.term_loc) added_term) &&
                              not (are_equiv (mk_constant_term_num_pos "0.0" t.term_loc) added_term) then
                             (
                               new_lhs_ts :=  added_term :: not_addable_terms
                             )
                           else
                                new_lhs_ts :=  not_addable_terms; 
                         )
                       | _ ->
                         (
                           new_lhs_ts := (List.nth replacing_term i) :: !new_lhs_ts
                         )
                     done;

                     match ( List.length  !new_lhs_ts) with
                     | 0 -> 
                       
                       { term_desc = TermQualIdentifierTerms(qual_id_rel, [(mk_constant_term_num_pos "0.0" t.term_loc); rhs]); term_loc = t.term_loc;}
                     | 1 -> 
                       
                       { term_desc = TermQualIdentifierTerms(qual_id_rel, [(List.hd !new_lhs_ts); rhs]); term_loc = t.term_loc;} 
                     | _ ->
                       ( 
                         let new_lhs = { term_desc = TermQualIdentifierTerms ((mk_qual_id "+" t.term_loc), !new_lhs_ts); term_loc = t.term_loc;} in                         
                           { term_desc = TermQualIdentifierTerms(qual_id_rel, [new_lhs; rhs]); term_loc = t.term_loc;}
                       );
                     
                   )
                 
                 | _ -> t
               )
             | false -> 
               (
                 let res_term = ref t in
                 let lhs_terms = ref (pre_LA_get_terms_rel_expr lhs) in
                   (   
                     let i = ref 1 in 
                     while !i < 100 do
                       let coeff_int = (string_of_int !i) in
                       let coeff_float = coeff_int ^ ".0" in  

                       (* try to identify add negative monoms *)
                       if (is_sub_term (pre_LA_get_opposite_term t1 coeff_int) lhs) then
                       (
                         lhs_terms := (pre_LA_new_lhs_terms t1 coeff_float coeff_int lhs_terms replacing_term);
                         i := 100
                       );

                       (* try to identify add positive monoms *)
                       if (is_sub_term (pre_LA_get_times_term t1 coeff_float) lhs) then
                       (
                         lhs_terms := (pre_LA_add_pos_monoms t1 coeff_float coeff_int lhs_terms replacing_term);
                         i := 100
                       );

                       i := !i + 1;
                     done; 

                     match ( List.length  !lhs_terms) with
                     | 0 -> 
                       (
                         { term_desc = TermQualIdentifierTerms(qual_id_rel, [(mk_constant_term_num_pos "0.0" t.term_loc); rhs]); term_loc = t.term_loc;} 
                       )
                     | 1 -> { term_desc = TermQualIdentifierTerms(qual_id_rel, [(List.hd !lhs_terms); rhs]); term_loc = t.term_loc;}
                     | _ ->
                       ( 
                         let new_lhs = { term_desc = TermQualIdentifierTerms ((mk_qual_id "+" t.term_loc), !lhs_terms); 
                                         term_loc = t.term_loc;} in
                          
                         { term_desc = TermQualIdentifierTerms(qual_id_rel, [new_lhs; rhs]); term_loc = t.term_loc;};
                       )
                   )
               )    
            )

          | 2 -> 
            let t1 = (List.nth to_replac_term 0) in
            let t2 = (List.nth to_replac_term 1) in
            (match (t1.term_desc, t2.term_desc) with
             | TermQualIdentifier _, TermQualIdentifierTerms (_, _) ->
               (
                 let res_term = ref t in
                   (
                     if (is_sub_term t1 lhs) && (is_sub_term t2 lhs) then
                       (
                         let lhs_terms = ref (pre_LA_get_terms_rel_expr lhs) in
                           (
                             lhs_terms :=  List.filter (fun x -> not (are_equiv x t1) && not (are_equiv x t2)) !lhs_terms;
                             for i=1 to ( List.length  replacing_term) do

                               if ( List.for_all  (fun x -> not (are_equiv (pre_LA_get_opposite_term (List.nth replacing_term (i-1)) "1") x)) !lhs_terms) then
                                 lhs_terms := (List.nth replacing_term (i-1)) :: !lhs_terms
                               else
                                 let terms_eqv = ( List.filter (fun x -> (are_equiv (pre_LA_get_opposite_term (List.nth replacing_term (i-1)) "1") x)) !lhs_terms) in
                                 match ( List.length  terms_eqv) with 
                                 | 1 -> lhs_terms := ( List.filter (fun x -> not (are_equiv (pre_LA_get_opposite_term (List.nth replacing_term (i-1)) "1") x)) !lhs_terms)
                                 | _ -> lhs_terms := (List.nth replacing_term (i-1)) :: !lhs_terms 
                             done;


                             (match ( List.length  !lhs_terms) with
                              | 0 -> res_term := (mk_constant_term_num_pos "0.0" t.term_loc)
                              | 1 -> res_term := { term_desc = TermQualIdentifierTerms(qual_id_rel, [(List.hd !lhs_terms); rhs]); term_loc = t.term_loc;}
                              | _ ->
                                (  
                                  let new_lhs = { term_desc = TermQualIdentifierTerms ((mk_qual_id "+" t.term_loc), !lhs_terms); 
                                                  term_loc = t.term_loc;} in
                                  res_term := { term_desc = TermQualIdentifierTerms(qual_id_rel, [new_lhs; rhs]);
                                                term_loc = t.term_loc;}
                                )
                             )
                           )
                       )
                     else
                      ( 
                         res_term := t;
                      );
                     

                     for j=1 to 100 do
                      
                       let coeff_int = (string_of_int j) in
                       let coeff_float = coeff_int ^ ".0" in  
                       let t1_op_int = (pre_LA_get_opposite_term t1 coeff_int) in
                       let t2_op_float = (pre_LA_get_opposite_term t2 coeff_float) in

                       let t1_op_float = (pre_LA_get_opposite_term t1 coeff_float) in
                       let t2_op_int = (pre_LA_get_opposite_term t2 coeff_int) in

                       let t1_times_int = (pre_LA_get_times_term t1 coeff_int) in
                       let t2_times_float = (pre_LA_get_times_term t2 coeff_float) in

                       let t1_times_float = (pre_LA_get_times_term t1 coeff_float) in
                       let t2_times_int = (pre_LA_get_times_term t2 coeff_int) in
                         
                       
	               if (is_sub_term t1_op_int  lhs) && (is_sub_term t2_op_float lhs) then
                         let lhs_terms = ref (pre_LA_get_terms_rel_expr lhs) in
                         let neg_replacing_terms = List.map (fun x -> (pre_LA_get_opposite_term x coeff_float)) replacing_term in
                         (
                           lhs_terms :=  List.filter (fun x -> not (are_equiv x t1_op_int) && not (are_equiv x t2_op_float)) !lhs_terms;
                           for i=1 to ( List.length  neg_replacing_terms) do
                             lhs_terms := (List.nth neg_replacing_terms (i-1)) :: !lhs_terms
                           done;
                     
                           let new_lhs = { term_desc = TermQualIdentifierTerms ((mk_qual_id "+" t.term_loc), !lhs_terms); 
                                           term_loc = t.term_loc;} in
                           res_term := { term_desc = TermQualIdentifierTerms(qual_id_rel, [new_lhs; rhs]);
                                         term_loc = t.term_loc;}; 
                         )
                       else res_term := t;


                       if (is_sub_term t1_op_float  lhs) && (is_sub_term t2_op_int lhs) then
                         let lhs_terms = ref (pre_LA_get_terms_rel_expr lhs) in
                         let neg_replacing_terms = List.map (fun x -> (pre_LA_get_opposite_term x coeff_float)) replacing_term in
                         (
                           lhs_terms :=  List.filter (fun x -> not (are_equiv x t1_op_float) && not (are_equiv x t2_op_int)) !lhs_terms;
                           for i=1 to ( List.length  neg_replacing_terms) do
                             lhs_terms := (List.nth neg_replacing_terms (i-1)) :: !lhs_terms
                           done;
                     
                           let new_lhs = { term_desc = TermQualIdentifierTerms ((mk_qual_id "+" t.term_loc), !lhs_terms); 
                                           term_loc = t.term_loc;} in
                           res_term := { term_desc = TermQualIdentifierTerms(qual_id_rel, [new_lhs; rhs]);
                                         term_loc = t.term_loc;}; 
                           
                         )
                       else res_term := t;

                       if (is_sub_term t1_times_int  lhs) && (is_sub_term t2_times_float lhs) then
                         let lhs_terms = ref (pre_LA_get_terms_rel_expr lhs) in
                         let neg_replacing_terms = List.map (fun x -> (pre_LA_get_opposite_term x coeff_float)) replacing_term in
                         (
                           lhs_terms :=  List.filter (fun x -> not (are_equiv x t1_times_int) && not (are_equiv x t2_times_float)) !lhs_terms;
                           for i=1 to ( List.length  replacing_term) do
                             lhs_terms := (pre_LA_get_times_term (List.nth replacing_term (i-1)) coeff_float)  :: !lhs_terms
                           done;
                     
                           let new_lhs = { term_desc = TermQualIdentifierTerms ((mk_qual_id "+" t.term_loc), !lhs_terms); 
                                           term_loc = t.term_loc;} in
                           res_term := { term_desc = TermQualIdentifierTerms(qual_id_rel, [new_lhs; rhs]);
                                         term_loc = t.term_loc;}; 
                         )
                       else res_term := t;

                       if (is_sub_term t1_times_float  lhs) && (is_sub_term t2_times_int lhs) then
                         let lhs_terms = ref (pre_LA_get_terms_rel_expr lhs) in
                         let neg_replacing_terms = List.map (fun x -> (pre_LA_get_opposite_term x coeff_float)) replacing_term in
                         (
                           lhs_terms :=  List.filter (fun x -> not (are_equiv x t1_times_float) && not (are_equiv x t2_times_int)) !lhs_terms;
                           for i=1 to ( List.length  replacing_term) do
                             lhs_terms := (pre_LA_get_times_term (List.nth replacing_term (i-1)) coeff_float) :: !lhs_terms
                           done;
                     
                           let new_lhs = { term_desc = TermQualIdentifierTerms ((mk_qual_id "+" t.term_loc), !lhs_terms); 
                                           term_loc = t.term_loc;} in
                           res_term := { term_desc = TermQualIdentifierTerms(qual_id_rel, [new_lhs; rhs]);
                                         term_loc = t.term_loc;}; 

                         )
                       else res_term := t;


                     done;
                   );
                   
                   !res_term
               )
            | _, _ -> t
            )
          | _ -> t 
         )
        else t;
      else t;
    )
;;

let rec pre_LA_var_replace_rel_term t l_pairs =
  match l_pairs with
  | [] -> t
  | p::ps ->
    ( 
      let terms = (pre_LA_get_terms_rel_expr t) in
      (* if (( List.length  (pre_LA_get_terms_rel_expr (List.hd terms))) = 2) then *)
      if false then
        t
      else
      (
        match ( List.length  terms) with
        | 2 ->
          (
            if (pre_LA_is_numeral_constant_term (List.nth terms 1)) && (float_of_string (pre_LA_get_numeral_const_expr (List.nth terms 1))) = 0.0 then
              (
                if (are_equiv (List.hd terms) (fst p)) then
                  pre_LA_var_replace_rel_term t ps
                else
                  (
                    match (pre_LA_get_term_operator (List.hd terms)) with
                    | "+" -> 
                      (
                        let simp_term = (pre_LA_simp_by_CC t (snd p)) in
                        (
                          let term_ret = pre_LA_var_replace_rel_term simp_term ps in
                          term_ret
                        )
                      )
                    | _ -> t
                  )
              )
            else raise (TermError "pre_LA_var_replace_rel_term: rhs not zero");
          )
        | _ -> raise (TermError "pre_LA_var_replace_rel_term:  List.length  of terms error")
      )
    )
;;


let rec pre_LA_var_replace_term  t =
  match t.term_desc with
  | TermLetTerm (_, _) -> raise (TermError "No let command expected")
  | TermSpecConstant _
  | TermQualIdentifier _
  | TermForallTerm (_, _)
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _) -> t
  | TermQualIdentifierTerms (qual_id, terms) -> 
    if (pre_LA_is_logic_oper(t)) then
      match (pre_LA_get_term_operator t) with
      | "and" ->
      (

        if ( List.for_all  pre_LA_is_rel_constraint terms) then
          (
           cc_list := [];

           let new_terms = ref [] in
           let ts = ref (List.nth terms 0) in
           for i = 0 to ( List.length  terms) - 1  do
             if (pre_LA_is_term (List.nth terms i) "=") then
             (
               (pre_LA_var_identify_replacing_pair (List.nth terms i));
             );


             if ( List.length  !cc_list > 0) then
             (
               (*
               print_string "\n----------------Term:\n";
               pp_pre_term Format.std_formatter (List.nth terms i);

               print_string "\n--\n";
               List.map (fun x -> (print_string "FIRST\n"); 
                             (List.map  (fun y -> (pp_pre_term Format.std_formatter y)) (fst (snd x))); 
                             (print_string "\nSECOND\n");
                             (List.map  (fun y -> (pp_pre_term Format.std_formatter y)) (snd (snd x))); 
                             (print_string "\n")) 
                   !cc_list;

               print_string "\n\n";
               *)

               ts := (pre_LA_var_replace_rel_term (List.nth terms i) !cc_list);
               
               (*
               print_string "\n---------------------Ts result\n";
               pp_pre_term Format.std_formatter !ts;
               *)

               if (pre_LA_is_term !ts "=") then
                 (pre_LA_var_identify_replacing_pair !ts);

               new_terms := List.append !new_terms [!ts];
             )
             else
               new_terms := List.append !new_terms [(List.nth terms i)];
           done;
               (*raise (TermError "end\n");*)

           (
             {term_desc = (TermQualIdentifierTerms(qual_id, !new_terms)); term_loc = t.term_loc;}  
           )
          )
        else
          (
            cc_list := [];
            {term_desc = (TermQualIdentifierTerms(qual_id, (List.map  pre_LA_var_replace_term terms))); term_loc = t.term_loc;};
          )
      )
      | _ -> 
        (
          let new_t = {term_desc = (TermQualIdentifierTerms(qual_id, (List.map  pre_LA_var_replace_term terms))); term_loc = t.term_loc;} in
          (*print_string "\n----------------Term:\n";
          pp_pre_term Format.std_formatter t;
          print_string "\n----------------RESULTS:\n";
          pp_pre_term Format.std_formatter new_t;
          
	  raise (TermError "\n--NANA--\n");
          *)
          {term_desc = (TermQualIdentifierTerms(qual_id, (List.map  pre_LA_var_replace_term terms))); term_loc = t.term_loc;};
        )
    else
    (
      if (pre_LA_is_rel_constraint t) then
        (
          (*
          print_string ((string_of_int ( List.length  !cc_list)) ^ "\n");
          List.map (fun x -> pp_pre_term Format.std_formatter (fst x)) !cc_list; 
          print_string "\n";
          *)
          (pre_LA_var_replace_rel_term t !cc_list)
        )
      else
        {term_desc = (TermQualIdentifierTerms(qual_id, (List.map  pre_LA_var_replace_term terms))); term_loc = t.term_loc;};
    ); 
;;

(* apply a simplifier function to an assert command (boolean term) *)
let apply_simp_fun cmd f =
  match cmd.command_desc with
  | CmdAssert t -> {command_desc = CmdAssert (f t);
                    command_loc = cmd.command_loc ;}
  | _ -> raise (TermError "No Assert command Error")
;;    

(* apply a simplifier function to an assert command (boolean term) *)
let apply_fun_script cmd f =
  match cmd.command_desc with
  | CmdAssert t -> (f t);
  | _ -> raise (TermError "No Assert command Error")
;;    








let rec pre_LA_save_var_ordering_term t =
  match t.term_desc with
  | TermLetTerm (_, _) -> raise (TermError "No let command expected")
  | TermQualIdentifier qualid -> 
    (match qualid.qual_identifier_desc with
     | QualIdentifierIdentifier id ->
       (match id.id_desc with
       | IdSymbol symbol 
       | IdUnderscore (symbol, _) ->          
         (match symbol.symbol_desc with
          | SimpleSymbol s
          | QuotedSymbol s ->
            try 
              (List.assoc s !var_ordering);
              () 
            with Not_found -> 
              (
                index_var_ord := (!index_var_ord + 1); 
                var_ordering := (List.append  !var_ordering [(s, !index_var_ord)])
              )
         )
       )
     | QualIdentifierAs (_, _) -> raise (TermError "ERROR: QualIdentifierAs")
    )  
  | TermQualIdentifierTerms (_, terms) -> 
    (
     (List.map  pre_LA_save_var_ordering_term terms);
     ()
    )
  | TermSpecConstant _ -> ()
  | TermForallTerm (_, _) 
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _) -> raise (TermError "ERROR: pre_LA_save_var_ordering_term: Nb occurs not yet implemented")
;;










let rec pre_LA_get_nb_occurs_term t =
  match t.term_desc with
  | TermLetTerm (_, _) -> raise (TermError "No let command expected")
  | TermQualIdentifier qualid -> 
    (match qualid.qual_identifier_desc with
     | QualIdentifierIdentifier id ->
       (match id.id_desc with
       | IdSymbol symbol 
       | IdUnderscore (symbol, _) ->          
         (match symbol.symbol_desc with
          | SimpleSymbol s
          | QuotedSymbol s -> 
            try 
              Hashtbl.replace !nb_occurs_res s ((Hashtbl.find !nb_occurs_res s) + 1)
            with Not_found -> Hashtbl.add !nb_occurs_res s 1
         )
       )
     | QualIdentifierAs (_, _) -> raise (TermError "ERROR: QualIdentifierAs")
    )  
  | TermQualIdentifierTerms (_, terms) -> 
    (
     (List.map  pre_LA_get_nb_occurs_term terms);
     ()
    )
  | TermSpecConstant _ -> ()
  | TermForallTerm (_, _) 
  | TermExistsTerm (_, _) 
  | TermAnnotatedTerm (_, _) -> raise (TermError "Nb occurs not yet implemented")
;;

let rec pre_LA_get_nb_occurs cmds =
  match cmds with
  | [] -> ()
  | cm::cms ->
    match cm.command_desc with
    | CmdAssert t -> 
      (
        (pre_LA_get_nb_occurs cms);
        (pre_LA_get_nb_occurs_term t)
      )
    | _ -> ()
;;    


let pre_LA_split_vars s =
       
      print_string "Before counting vars\n";
      (pre_LA_get_nb_occurs s);
      print_string "After counting vars\n";
   
      let nb_occurs = ref (Hashtbl.fold (fun k v acc -> (k, v) :: acc) !nb_occurs_res []) in

      print_string ("After counting vars 500 " ^ (string_of_int ( List.length  !nb_occurs)) ^ "\n");
 
      (* let max_nb_oc = ref (List.fold_left (max) 0 (List.map (fun x -> (snd x)) !nb_occurs)) in *)
      (* let min_nb_oc = ref (List.fold_left (min) max_int (List.map (fun x -> (snd x)) !nb_occurs)) in *)

      let max_nb_oc = ref 0 in
      let min_nb_oc = ref max_int in
      for i = 0 to ( List.length  !nb_occurs) - 1 do
        if  (snd (List.nth !nb_occurs i)) > !max_nb_oc then
         max_nb_oc := (snd (List.nth !nb_occurs i));
        if  (snd (List.nth !nb_occurs i)) < !min_nb_oc then
         min_nb_oc := (snd (List.nth !nb_occurs i));
      done;
      

      print_string "After counting vars 1000\n";

      let m = ref ((!min_nb_oc + !max_nb_oc) / 2) in
      let m1 = ref ((!m + !max_nb_oc) / 2) in
      print_string "After counting vars 2000\n";

      for i = 0 to ( List.length  !nb_occurs) - 1 do
        if  (snd (List.nth !nb_occurs i)) > !m1 then
          global_vars := List.append !global_vars [(fst (List.nth !nb_occurs i))]
        else
          local_vars := List.append !local_vars [(fst (List.nth !nb_occurs i))];
      done;

      (*
      global_vars := (List.map  (fun x -> (fst x)) ( List.filter (fun x -> (snd x) >= !m1) !nb_occurs));
      print_string "After counting vars3000\n";

      local_vars := (List.map  (fun x -> (fst x)) ( List.filter (fun x -> (snd x) < !m1) !nb_occurs));
      print_string "After counting vars 4000\n";
      *)
;;


let pre_LA_print_vars_distrib s =
  let nb_occurs = ref (Hashtbl.fold (fun k v acc -> (k, v) :: acc) !nb_occurs_res []) in
  
  let s_assert_1 = (List.map (fun c -> apply_simp_fun c pre_LA_subs_let_expr_term) s) in 
  (pre_LA_split_vars s_assert_1);
 
  List.iter (fun x -> (print_string ((fst x) ^ "\t" ^ (string_of_int (snd x)) ^ "\n") )) !nb_occurs;
  print_string "\n--\n";

;;

let pre_LA fmt (s: Ast.script) =
  let s_pre = (List.filter pre_LA_is_decl_def s.script_commands) in
  let s_post = (List.filter pre_LA_is_check_exit s.script_commands) in 
  let s_assert = (List.filter pre_LA_is_assert s.script_commands) in           
  
  let s_declare = (List.filter pre_LA_is_declare s.script_commands) in
  let s_define = (List.filter pre_LA_is_define s.script_commands) in
  let s_declare_real = (List.filter (fun x -> (pre_LA_is_declare_type x "Real")) s.script_commands) in

  match ( List.length  s_define) > 0  || not (( List.length  s_declare) = ( List.length  s_declare_real)) with 
  | true -> 
    (
      pp_tofile "out.smt2" { script_commands = (List.append s_pre (List.append s_assert s_post)) ; 
                                   script_loc = s.script_loc;} 
    )
  | _ ->
    (

      let s_assert_1 = (List.map (fun c -> apply_simp_fun c pre_LA_subs_let_expr_term) s_assert) in      

      let s_assert_2 = (List.map (fun c -> apply_simp_fun c pre_LA_trivial_simp_term) s_assert_1) in
      let s_assert_flat = (List.map (fun c -> apply_simp_fun c pre_flat_term) s_assert_2) in      

      let s_assert_sort = (List.map (fun c -> apply_simp_fun c pre_LA_sort_terms) s_assert_flat) in    

      print_string "After sort\n";

      let s_assert_3 = (List.map (fun c -> apply_simp_fun c pre_LA_trivial_rel_merge) s_assert_sort) in 

      print_string "Before spliting vars\n";

      (pre_LA_split_vars s_assert_3);

      raise (TermError "End");
      
      cc_list := [];

      let s_assert_41 = (List.map (fun c -> apply_simp_fun c pre_LA_trivial_simp_term) s_assert_3) in

      let s_assert_4 = (List.map (fun c -> apply_simp_fun c (fun y -> cc_list := []; (pre_LA_var_replace_term y))) s_assert_41) in 
      (* let s_assert_4 = (List.map (fun c -> apply_simp_fun c pre_LA_var_replace_term) s_assert_41) in *)

      pp_tofile "out.smt2" {script_commands = (List.append s_pre (List.append s_assert_4 s_post)) ; 
                            script_loc = s.script_loc;}; 
  )
;;


let pre_NLA fmt (s: Ast.script) =
  let s_pre = (List.filter pre_LA_is_decl_def s.script_commands) in
  let s_post = (List.filter pre_LA_is_check_exit s.script_commands) in 
  let s_assert = (List.filter pre_LA_is_assert s.script_commands) in            
  let s_declare = (List.filter pre_LA_is_declare s.script_commands) in
  let s_define = (List.filter pre_LA_is_define s.script_commands) in
  let s_declare_real = (List.filter (fun x -> (pre_LA_is_declare_type x "Real")) s.script_commands) in

  let s_assert_1 = (List.map (fun c -> apply_simp_fun c pre_LA_subs_let_expr_term) s_assert) in      
  let s_assert_2 = (List.map (fun c -> apply_simp_fun c pre_LA_trivial_simp_term) s_assert_1) in


  let s_assert_3 = (List.map (fun c -> apply_simp_fun c pre_flat_term) s_assert_2) in      
  let s_assert_4 = (List.map (fun c -> apply_simp_fun c pre_term_distribution) s_assert_3) in      
  let s_assert_5 = (List.map (fun c -> apply_simp_fun c pre_flat_term) s_assert_4) in      
  let s_assert_6 = (List.map (fun c -> apply_simp_fun c pre_term_clear_rhs) s_assert_5) in 
  let s_assert_7 = (List.map (fun c -> apply_simp_fun c pre_LA_trivial_rel_merge) s_assert_6) in 
  (
    (List.map (fun c -> (apply_fun_script c pre_LA_save_var_ordering_term)) s_assert_5);
    let s_assert_sort = (List.map (fun c -> apply_simp_fun c pre_LA_sort_terms) s_assert_6) in    
    pp_tofile "out_before.smt2" { script_commands = (List.append s_pre (List.append s_assert_6 s_post)) ; 
                                  script_loc = s.script_loc;}; 
    pp_tofile "out_after.smt2" { script_commands = (List.append s_pre (List.append s_assert_7 s_post)) ; 
                                 script_loc = s.script_loc;};  


    (List.map  (fun x -> print_string ((fst x) ^ "\t" ^ (string_of_int (snd x)) ^ "\n")) !var_ordering);
    print_string "After sort\n";
  )
;;
