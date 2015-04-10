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

let all_symbol_strings (t : t) : Utils.StringSet.t =
  let sortset =
    List.fold_left
      (fun set so -> Utils.StringSet.union set (Sorts.basic_sorts so))
      Utils.StringSet.empty t.theory_sorts
  in
  StringSet.union sortset (StringSet.of_list (List.map fst t.theory_symbols))
;;


module type Theory = sig
    val theory : t ;;
end

module EmptyTheory = struct
    let theory = { theory_symbols = []; theory_sorts = []; } ;;
end

module SMTCore = struct
    let sorts = [ bool_sort; ] ;;

    let boolbool_bool_fun = xx_y_fun bool_sort bool_sort ;;

    let symbols =
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

    let theory = mk_theory sorts symbols ;;
end

module type Arithmetics = sig
    val base_sort : Sorts.t ;;
    val local_binary_arith_ops : (string * Logic.arith_kind) list ;;
    val local_unary_arith_ops : string list ;;
end

module SharedArithmetics (A : Arithmetics) = struct
    open Logic ;;
    open Ast ;;
    let sorts = [ A.base_sort; ] ;;

    let tt_bool_fun = xx_y_fun A.base_sort bool_sort ;;
    let tt_t_fun = xx_y_fun A.base_sort A.base_sort ;;
    let t_t_fun = x_y_fun A.base_sort A.base_sort ;;

    (* Common symbols for real or integer arithmetic *)
    let binary_arith_ops =
      [("-", Difference);
       ("*", NonLinear);
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

    let is_multiplication (id : Ast.identifier) =
      match id.id_desc with
      | IdSymbol ( { symbol_desc = SimpleSymbol name; _ }) ->
         String.compare name "*" = 0
      | IdSymbol _
      | IdUnderscore _ -> false
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

module SMTBitVectors = struct
    let sorts = [ bitvector_sort ] ;;

    let symbols = [
        "bvand", unit_sort;
        "bvor", unit_sort;
        "bvneg", unit_sort;
        "bvnot", unit_sort;
        "bvadd", unit_sort;
        "bvsub", unit_sort;
        "bvlshr", unit_sort;
        "bvlshl", unit_sort;
        "bvshr", unit_sort;
        "bvshl", unit_sort;
        "sign_extend", unit_sort;
        "concat", unit_sort;
        "extract", unit_sort;
      ]
    ;;

    let theory =
      { theory_symbols = symbols;
        theory_sorts = sorts;
      }
    ;;
end

module SMTArray = struct
    let sorts = [ array_sort;  ] ;;

    let symbols =
      [ "select", unit_sort;
        "store", unit_sort;
      ]
    ;;

    let theory =
      { theory_symbols = symbols;
        theory_sorts = sorts;
      }
    ;;
end
