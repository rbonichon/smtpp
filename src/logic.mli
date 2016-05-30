type arith_sort = Integer | Real | Mixed ;;
(* The basic sort upon which arithmetic can be based *)

type arith_kind = Difference | Linear | NonLinear ;;
(* Kind of arithmetics used in SMT-LIB benchmarks.
   They form a simple lattice.
*)

val max_kind_opt : arith_kind option -> arith_kind option -> arith_kind option

type t = {
    smt_name : string;
    mutable array : bool ; (* Array theory on / off ? *)
    mutable uninterpreted_functions : bool ;
    mutable bitvectors : bool ;
    mutable quantifiers : bool ;
    mutable arithmetic_sort : arith_sort option ;
    mutable arithmetic_kind : arith_kind option ;
  }
;;

val default : t

val parse_logic : string -> t

val pp_from_core : Format.formatter -> t -> unit
