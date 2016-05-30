type t = {
  theory_sorts : Sorts.t list ;
  theory_symbols : (string * Sorts.t) list ;
}
  

val combine : t -> t -> t
val all_symbol_strings : t -> Utils.StringSet.t * Utils.StringSet.t 


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
;;

module Make(TDef: TheoryDefinition) : Theory ;;

module SMTCore : Theory
module SMTArray : Theory
module SMTBitVectors : Theory


module CommonArithmetics : sig
  val is_multiplication : Ast.identifier -> bool 
end

module SMTInt : sig
  val theory : t
  val kind_map : Logic.arith_kind Utils.StringMap.t
end
;;

module SMTReal : sig
  val theory :  t
  val kind_map : Logic.arith_kind Utils.StringMap.t
end
;;

module SMTNumerics : sig
  val theory :  t
end
;;



