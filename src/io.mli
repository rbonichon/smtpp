
val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
val log : ('a, Format.formatter, unit, unit) format4 -> 'a
val result : ('a, Format.formatter, unit, unit) format4 -> 'a
val warning : ('a, Format.formatter, unit, unit) format4 -> 'a
val fail : Locations.t -> string -> 'a

val set_tagging : bool -> unit

module Error : sig
  val report : Lexing.lexbuf -> string -> 'a
end

val not_yet_implemented : string -> 'a
