open Logic ;;

type script = {
    script_commands : Ast.commands ;
    script_loc : Locations.t ;
    script_theory : Theory.t ;
  }
;;

let to_ast_script (s : script) =
  { Ast.script_commands = s.script_commands;
    Ast.script_loc = s.script_loc;
  }
;;

let load_theories (s : Ast.script) : script =
  let theory = ref Theory.SMTCore.theory in
  let add_theory (th : Theory.t) =
    theory := Theory.combine !theory th;
  in
  let logic_name = Ast_utils.get_logic s in
  let t = parse_logic logic_name in
  (* if t.aradd_theory t.array Theory.EmptyTheory.theory; (* FIXME *)*)
  if t.bitvectors then add_theory Theory.SMTBitVectors.theory;
  (match t.arithmetic_sort with
   | Some Integer -> add_theory Theory.SMTInt.theory 
   | Some Real -> add_theory Theory.SMTReal.theory
   | Some Mixed -> add_theory Theory.SMTNumerics.theory
   | None -> ()
  );
  { script_commands = s.Ast.script_commands;
    script_loc = s.Ast.script_loc;
    script_theory = !theory;
  }
;;
