val pp: Format.formatter -> Ast.script -> unit ;;
(** [pp fmt ast] pretty-prints a full SMT-LIB script onto a formatter *)

val pp_commands: Format.formatter -> Ast.commands -> unit ;;
(** pp_commands pretty_prints an arbitrary command list onto a formatter.
    Used by pp.
 *)

val pp_tofile: string -> Ast.script -> unit ;;
(** [pp_tofile filename script] Prints a SMT-LIB script into the file named
 ** [filename]. The file is created if needed. Contents from any present file is
 ** not preserved.
 *)
