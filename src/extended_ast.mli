type script = {
    script_commands : Ast.commands ;
    script_loc : Locations.t ;
    script_theory : Theory.t ;
  }
;;

val to_ast_script : script -> Ast.script ;;

val load_theories : Ast.script -> script ;;
