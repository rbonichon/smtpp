open Ast ;;
open Extended_ast ;;

let eval_command =
  let last_sat_status_info = ref None in
  let set_sat_status_info cmd =
    match cmd.command_desc with
    | CmdSetInfo attr ->
       begin
         match attr.attribute_desc with
         | AttrKeyword _ -> ()
         | AttrKeywordValue (kwd, attr_value) ->
            if Bytes.compare kwd "status" = 0 then
              begin
                match attr_value.attr_value_desc with
                | AttrValSexpr _ | AttrValSpecConstant _ -> ()
                | AttrValSymbol symb ->
                   begin
                     match symb.symbol_desc with
                     | SimpleSymbol str
                     | QuotedSymbol str ->
                        last_sat_status_info := Some str;
                   end
              end
       end
    | _ -> ()
  in
  fun states cmd ->
  match cmd.command_desc with
  | CmdCheckSat ->
     begin
       match states with
       | [] ->
          begin
            Io.warning "(check-sat) on line %a has no status@."
                       Pp.pp_loc cmd.command_loc;
            [cmd], []
          end
       | st :: sts ->
          begin
            match !last_sat_status_info with
            | None ->
               let satinfo = Ast_utils.mk_sat_info ~loc:cmd.command_loc st in
               [cmd; satinfo], sts
            | Some status ->
               if Bytes.compare status st <> 0 then (
                 Io.error "Sat status already set to %s (change was to %s)@."
                          status st;
                 exit 3;)
               else [cmd], sts
          end

     end
  | _ ->
     set_sat_status_info cmd;
     [cmd], states
;;

let add_sat_status (script : ext_script) =
  let status_list = Config.get_sat_status () in
  match status_list with
  | None -> script
  | Some status_list ->
     let ext_script_commands, status_list =
       List.fold_left
         (fun (cmds, sts) cmd ->
          let mycmds, mysts = eval_command sts cmd in
          mycmds @ cmds, mysts
         ) ([], status_list) script.ext_script_commands
     in
     if status_list <> [] then
       Io.warning
         "%d sat information were not used@."
         (List.length status_list);
     let ext_script_commands = List.rev ext_script_commands in
     { script with ext_script_commands; }
;;
