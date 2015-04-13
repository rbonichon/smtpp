val lex_file : string -> Lexing.lexbuf * (unit -> unit) ;;

val apply : unit -> unit ;;
  (** Apply the parser and subsequent analyses *)
