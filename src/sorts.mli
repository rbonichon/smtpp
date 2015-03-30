(** {2} Type definition *)
type sort_identifier = string
type arity = int

type t = private
  | Basic of sort_identifier * arity
  | App of t * t list
  | Fun of t list * t
  | Var of string
  | Poly of string list * t
;;

(** Pre-defined sorts *)
val int_sort : t
val bool_sort : t
val real_sort : t
val unit_sort : t
val array_sort : t
val bitvector_sort : t
;;

val pp_sort : Format.formatter -> t -> unit ;;
(** [pp_sort fmt sort] pretty-prints the sort [sort] in the formatter [fmt] *)

(** {2} Construction functions *)
val mk_poly : string list -> t -> t ;;
val mk_var : unit -> t ;;
val mk_basic : string -> int -> t ;;
val mk_app : t -> t list -> t ;;
(** [mk_app sort sorts] applies the sort [sort] to an argument sort list
 ** [sorts].
 ** This construction function verifiers that the application is valid,
 ** i.e. that the arity of [sort] is the same as the length of [sorts].
 ** If not, it raises Invalid_argument.
 *)

val mk_fun : t list -> t -> t ;;
val generalize : t -> t ;;
(** [generalize sort] will generalize the sort [sort]. That is, all free
 ** variables will be taken to be universally quantified.
 *)

val generics : string -> int -> t ;;
(** [generics name arity] create a basic polymorphic sort of name [name] with
 ** [arity] polymorphic variables. This sort is closed and has no free
 ** variables.
 *)

(** Helpers *)
val xx_y_fun : t -> t -> t
(** [xx_y_fun s1 s2] creates a function sort  x * x -> y *)
val x_y_fun : t -> t -> t
(** [x_y_fun s1 s2] creates a function sort  x -> y *)

(** {2} Misc *)
val get_basic_sort_name : t -> sort_identifier
(** [get_basic_sort_name s]
    - returns the sort_identifiers if [s] is Basic(_, _),
    - assert false otherwise
 *)

val basic_sorts : t -> Utils.StringSet.t
(** [basic_sorts s] returns a set of the names of all ground sorts of s *)

module SortSet : sig
    include Set.S
end
