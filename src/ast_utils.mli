(** { 1 }  Transformations *)
val symbol_of_svar : Ast.sorted_var -> Ast.symbol ;;
  (** Extracts the symbol part out of the declaration of a sorted variable. *)

val sort_of_svar : Ast.sorted_var -> Ast.sort ;;
  (** Extracts the sort part out of the declaration of a sorted variable. *)

val symbol_of_vbinding : Ast.var_binding -> Ast.symbol ;;
  (** Extracts the newly defined symbol part out of a variable binding. *)

val symbols_of_sort : Ast.sort ->  Ast.symbol list ;;
val string_of_symbol : Ast.symbol -> string ;;
val symbol_of_id : Ast.identifier -> Ast.symbol ;;

val get_logic : Ast.script -> string ;;
  (** Extracts the logic name from a SMT script *)

val id_of_qid : Ast.qual_identifier -> Ast.identifier ;;
  (** [id_from_qid qid] extracts the id part of a qualified identifier *)


(** { 2 } Construction functions *)
val mk_symbol : ?loc:Locations.t -> string -> Ast.symbol ;;
(** [mk_symbol ~loc name] creates a dummy symbol for name
 ** if loc is not set, a dummy location will be used.
 *)

val mk_command: Ast.command_desc -> Ast.command ;;
(** [mk_command cmd_des] creates a command with a dummy location *)

val mk_sat_info : ?loc:Locations.t -> string -> Ast.command ;;
(** [mk_sat_info ~loc sat_status] creates (set-info :status sat_status)
 ** command.
 ** if loc is not set, a dummy location will be used.
 *)
