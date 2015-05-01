type ext_script = {
    ext_script_commands : Ast.commands ;
    ext_script_loc : Locations.t ;
    ext_script_theory : Theory.t ;
  }
;;

val to_ast_script : ext_script -> Ast.script ;;

val load_theories : Ast.script -> ext_script ;;

val set_logic : Logic.t -> ext_script -> ext_script ;;
