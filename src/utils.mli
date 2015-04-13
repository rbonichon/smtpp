module StringMap : sig
    include Map.S with type key = string ;;
end

module StringSet : sig
   include  Set.S with type elt = string ;;
   val to_list : t -> elt list ;;
end
;;

module SymbolSet : Set.S with type elt = Ast.symbol ;;

val sfprintf : ('a, Format.formatter, unit, string) format4 -> 'a ;;

val mk_header : Format.formatter -> string -> unit ;;


val string_explode : char -> string -> string list ;;

val has_more_than : int -> ('a -> bool) -> 'a list -> bool ;;

val opt_compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int ;;
(** [opt_compare compare e1_opt e2_opt] lifts a comparison function defined
 ** on some base type to the optional type for this base type.
 ** If both types are None, they are equal. If both are Some, the provided
 ** comparison function is used. Otherwise, the Some _ element is considered
 ** bigger than the None element.
 *)

val default_opt : 'a -> 'a option -> 'a ;;
(** [default_opt default_value opt_value] returns the value of the optional
 ** value if it is Some v, [default_value] otherwise.
 *)
