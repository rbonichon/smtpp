open Sorts ;;

module type Theory = sig
    val sorts : Sorts.t list ;;
    val symbols : (string * Sorts.t) list ;;
end

module Combine(T1 : Theory)(T2 : Theory) : Theory = struct
    let sorts = T1.sorts @ T2.sorts ;;
    let symbols = T1.symbols @ T2.symbols ;;
end

module type CheckableTheory = sig
    include Theory ;;
    include Checks.Check ;;
    val set : bool -> unit ;;
    val get : unit -> bool ;;
end

module EmptyTheory = struct
    let sorts = [] ;;
    let symbols = [] ;;
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
end

(* Mixed type accepting both integers and reals *)
module SMTNumerics = struct
    include Combine(SMTInt)(SMTReal) ;;

    let symbols = symbols
      @ ["to_real", x_y_fun int_sort real_sort;
         "to_int", x_y_fun real_sort int_sort;
         "is_int", x_y_fun real_sort bool_sort;
        ]
    ;;
end

module SMTBitVectors = struct
    include EmptyTheory ;;
end
