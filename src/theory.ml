open Sorts ;;

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

let all_symbol_strings (t : t) =
  (List.map Sorts.basic_sort_name t.theory_sorts)
  @ List.map fst t.theory_symbols
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
       "not"     , Fun([bool_sort;], bool_sort);
       "and"     , boolbool_bool_fun;
       "or"      , boolbool_bool_fun;
       "xor"     , boolbool_bool_fun;
       "="       , Poly(["X"], Fun([Var "X"; Var "X"], bool_sort));
       "distinct", Poly(["X"], Fun([Var "X"; Var "X"], bool_sort));
       "ite"     , Poly(["X"], Fun([Var "X"; Var "X"], Var "X"));
      ]
    ;;

    let theory = mk_theory sorts symbols ;;
end

module SMTInt = struct
    let sorts = [ int_sort; ] ;;

    let symbols =
      let intint_bool_fun = xx_y_fun int_sort bool_sort in
      let intint_int_fun = xx_y_fun int_sort int_sort in
      let int_int_fun = x_y_fun int_sort int_sort in
      ["-"  , int_int_fun; "abs", int_int_fun; ]
      @ List.map (fun e -> (e, intint_int_fun)) ["-"; "*"; "+"; "div"; "mod";]
      @ List.map (fun e -> (e, intint_bool_fun)) ["<="; "<"; ">="; ">";]
    ;;

   let theory = mk_theory sorts symbols ;;
end

module SMTReal = struct
    let sorts = [ real_sort; ] ;;

    let real_real_fun = Fun([real_sort;], real_sort) ;;
    let realreal_real_fun = xx_y_fun real_sort real_sort ;;
    let realreal_bool_fun = xx_y_fun real_sort bool_sort ;;

    let symbols =
      ["-"  , real_real_fun; ]
      @ List.map (fun e -> (e, realreal_real_fun)) ["-"; "*"; "+"; "/";]
      @ List.map (fun e -> (e, realreal_bool_fun)) ["<="; "<"; ">="; ">";]
    ;;

    let theory = mk_theory sorts symbols ;;
end

(* Mixed type accepting both integers and reals *)
module SMTNumerics = struct
    let symbols =
      ["to_real", x_y_fun int_sort real_sort;
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
    include EmptyTheory ;;
end
