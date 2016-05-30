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

open Sorts ;;
open Utils ;;

type t = {
    theory_sorts : Sorts.t list ;
    theory_symbols : (string * Sorts.t) list ;
  }
;;

let mk_theory theory_sorts theory_symbols =
  { theory_symbols; theory_sorts }
;;

let combine (t1 : t) (t2 : t) =
  { theory_sorts = t1.theory_sorts @ t2.theory_sorts;
    theory_symbols = t1.theory_symbols @ t2.theory_symbols ;
  }
;;

let all_symbol_strings (t : t) : Utils.StringSet.t * Utils.StringSet.t =
  let sortset =
    List.fold_left
      (fun set so -> Utils.StringSet.union set (Sorts.basic_sorts so))
      Utils.StringSet.empty t.theory_sorts
  in sortset, StringSet.of_list (List.map fst t.theory_symbols)
;;

module type TheoryDefinition = sig
    val name : string ;;
    val sorts : Sorts.t list ;;
    val functions : (string * Sorts.t) list ;;
  end
;;

module type Theory = sig
    val name : string ;;
    val theory : t ;;
    val mem : Ast.symbol -> bool ;;
end

module Make(TDef : TheoryDefinition) : Theory = struct
    let name = TDef.name ;;
    let theory = mk_theory TDef.sorts TDef.functions ;;
    let sortnames, function_names = all_symbol_strings theory ;;
    let mem (sy : Ast.symbol) =
      let symbol_name = Ast_utils.string_of_symbol sy in
      StringSet.mem symbol_name sortnames
      || StringSet.mem symbol_name function_names
    ;;
  end


module EmptyTheory = struct
    let _theory = { theory_symbols = []; theory_sorts = []; } ;;
end

(** Definition for SMT Core *)
module SMTCoreDefinition = struct
    let name = "Core" ;;
    let sorts = [ bool_sort; ] ;;

    let boolbool_bool_fun = xx_y_fun bool_sort bool_sort ;;

    let functions =
      ["true"    , bool_sort ;
       "false"   , bool_sort ;
       "not"     , x_y_fun bool_sort bool_sort;
       "and"     , boolbool_bool_fun;
       "or"      , boolbool_bool_fun;
       "=>"      , boolbool_bool_fun;
       "xor"     , boolbool_bool_fun;
       "="       , generalize (xx_y_fun (mk_var ()) bool_sort);
       "distinct", generalize (xx_y_fun (mk_var ()) bool_sort);
       "ite"     , let v = mk_var () in mk_fun [bool_sort; v; v;] v;
      ]
    ;;
end

module SMTCore = Make(SMTCoreDefinition) ;;

(** Definition for various types of arithmetic *)
 module type Arithmetics = sig
    val base_sort : Sorts.t ;;
    val local_binary_arith_ops : (string * Logic.arith_kind) list ;;
    val local_unary_arith_ops : string list ;;
end

module CommonArithmetics = struct
    open Ast ;;
    let is_multiplication (id : Ast.identifier) =
      match id.id_desc with
      | IdSymbol ( { symbol_desc = SimpleSymbol name; _ }) ->
         String.compare name "*" = 0
      | IdSymbol _
      | IdUnderscore _ -> false
    ;;
end

module SharedArithmetics (A : Arithmetics) = struct
    open Logic ;;

    include CommonArithmetics ;;
    let sorts = [ A.base_sort; ] ;;

    let tt_bool_fun = xx_y_fun A.base_sort bool_sort ;;
    let tt_t_fun = xx_y_fun A.base_sort A.base_sort ;;
    let t_t_fun = x_y_fun A.base_sort A.base_sort ;;

    (* Common symbols for real or integer arithmetic *)
    let binary_arith_ops =
      [("-", Difference);
       ("*", Linear); (* If the multiplication has only one variable, it is
                       * linear *)
       ("+", Linear);
      ]
      @ A.local_binary_arith_ops
    ;;

    let unary_arith_ops = "-" :: A.local_unary_arith_ops ;;

    let relations = ["<="; "<"; ">="; ">";] ;;

    let typed_symbols =
      List.map (fun e -> (e, t_t_fun)) unary_arith_ops
      @ List.map (fun (e, _) -> (e, tt_t_fun)) binary_arith_ops
      @ List.map (fun e -> (e, tt_bool_fun)) relations
    ;;

    let kind_map : Logic.arith_kind StringMap.t =
      List.fold_left
        (fun m (name, kind) -> StringMap.add name kind m)
        (List.fold_left
           (fun m name -> StringMap.add name Difference m)
           StringMap.empty relations)
        binary_arith_ops
    ;;

    let theory = mk_theory sorts typed_symbols ;;
end

module SMTInt = SharedArithmetics(
                struct
                  let base_sort = int_sort ;;
                  let local_binary_arith_ops =
                    [ ("div", Logic.NonLinear); ("mod", Logic.NonLinear); ]
                  ;;
                  let local_unary_arith_ops = [ "abs"; ]
                end) ;;

module SMTReal =  SharedArithmetics(
                struct
                  let base_sort = real_sort ;;
                  let local_binary_arith_ops = [ ("/", Logic.NonLinear);] ;;
                  let local_unary_arith_ops = [] ;;
                end) ;;

(* Mixed type accepting both integers and reals *)
module SMTNumerics = struct
    let symbols =
      [ "to_real", x_y_fun int_sort real_sort;
        "to_int", x_y_fun real_sort int_sort;
        "is_int", x_y_fun real_sort bool_sort;
      ]
    ;;

    let theory =
      let t = combine SMTInt.theory SMTReal.theory in
      { t with theory_symbols = t.theory_symbols @ symbols }
    ;;
end

(** BitVectors *)
module SMTBitVectorsDefinition = struct
    let name = "BitVectors" ;;
    let sorts = [ bitvector_sort ] ;;

    let functions = [
        "bvand", unit_sort;
        "bvnand", unit_sort;
        "bvor", unit_sort;
        "bvneg", unit_sort;
        "bvnot", unit_sort;
        "bvadd", unit_sort;
        "bvsub", unit_sort;
        "bvmul", unit_sort;
        "bvudiv", unit_sort;
        "bvurem", unit_sort;
        "bvlshr", unit_sort;
        "bvlshl", unit_sort;
        "bvshr", unit_sort;
        "bvshl", unit_sort;
        "sign_extend", unit_sort;
        "concat", unit_sort;
        "extract", unit_sort;
        "zero_extend", unit_sort;
        (* Unsigned comparison operators *)
        "bvuge", unit_sort;
        "bvugt", unit_sort;
        "bvule", unit_sort;
        "bvult", unit_sort;
        (* Signed comparison operators *)
        "bvslt", unit_sort;
        "bvsle", unit_sort;
        "bvsgt", unit_sort;
        "bvsge", unit_sort;
        "bv1", unit_sort;
        "bv0", unit_sort;
      ]
    ;;
end

module SMTBitVectors = Make(SMTBitVectorsDefinition) ;;

(** Array *)
module SMTArrayDefinition = struct
    let name = "Array" ;;
    let sorts = [ array_sort;  ] ;;

    let functions =
      [ "select", unit_sort;
        "store", unit_sort;
      ]
    ;;
end

module SMTArray = Make(SMTArrayDefinition) ;;
