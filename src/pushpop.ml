(**************************************************************************)
(*  Copyright (c) 2015 Richard Bonichon <richard.bonichon@gmail.com>      *)
(*                                                                        *)
(*  Permission to use, copy, modify, and distribute this software for any  *)
(*  purpose with or without fee is hereby granted, provided that the above  *)
(*  copyright notice and this permission notice appear in all copies.     *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  *)
(*  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF      *)
(*  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *)
(*  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES  *)
(*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN  *)
(*  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *)
(*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.        *)
(*                                                                        *)
(**************************************************************************)

open Ast ;;
open Format ;;
open Lexing ;;
open Locations ;;

(* Extract from the SMT-LIB reference v. 2.0 p.36 - 37
  Conforming solvers respond to various commands by performing
  operations on a data structure called the assertion-set stack. This is a
  single global stack, where each element on the stack is a set of
  assertions. Assertions include both logical formulas
  (that is, terms of Boolean type), as well as declarations and definitions of
  sort symbols and function symbols. Such declarations and definitions are thus
  local: popping an assertion set from the assertion-set stack removes all
  declarations and definitions contained in that set. This feature supports the
  removal of definitions and declarations, without recourse to
  undefining or shadowing, neither of which are supported or allowed.
 *)

(* Semantics (p.57-58)
 *
 * Growing the stack: the command push n pushes n empty assertion sets
 * (typically, 1) onto the stack. If n is 0, no assertion sets are pushed.
 *
 * Shrinking the stack: The command pop n pops the top n assertion sets from
 * the stack. If n is greater than or equal to the current stack depth, an error
 * results. 1 If n is 0, no assertion sets are popped.
 *)

let set_basename, new_file_name =
  let i = ref 0 in
  let basename = ref "smt_pop" in
  (fun filename ->
   let bname = Filename.basename filename in
   basename := Filename.chop_extension bname) ,
  (fun () -> incr i; sprintf "%s_%d.smt2" !basename !i)
;;

type assertion_set = command list ;;
let empty_assertion_set : assertion_set list = [] ;;
exception EmptyStack ;;

(* Only declarations and definitions should be kept and pushed *)
let push cmds stack =
  let should_be_pushed cmd =
    match cmd.command_desc with
    | CmdAssert _
    | CmdDefineFun _
    | CmdDeclareFun _
    | CmdDefineSort _
    | CmdDeclareSort _ -> true
    | _ -> false
  in (List.filter should_be_pushed cmds) :: stack
;;

let pop n stack =
  assert (n >= 0);
  if n = 0 then stack
  else
    let rec aux n stack =
      match stack with
      | [] -> if n != 0 then raise EmptyStack else stack
      | _ :: xs -> aux (n - 1) xs
    in aux n stack
;;

let stack_script stack = List.flatten stack
;;

let exit_cmd = Ast_utils.mk_command CmdExit ;;

(* Generates a list of n empty list *)
let mk_empty_pushes n =
  assert (n >= 0);
  let rec aux s m =
    if m = 0 then s
    else aux ([] :: s) (m - 1)
  in aux [] n
;;

let eval (prelude, stack, cmds) cmd =
  match cmd.command_desc with
  | CmdPush n ->
     begin
       let n = int_of_string (Utils.default_opt "1" n) in
       assert (n >= 0);
       match n with
       | 0 -> prelude, stack, cmds
       | 1 -> prelude, push cmds stack, []
       | _  ->
          (* The current command list (filtered) is saved on the stack.
           * Empty command lists are added as needed to make the count.
           *)
          if n > 1 then
            let stack' = (mk_empty_pushes (n - 1)) @ push cmds stack in
            prelude, stack', []
          else assert false
     end

  | CmdPop n ->
     let n = int_of_string (Utils.default_opt "1" n) in
     assert (n >= 0);
     (* Substitutes the current environment by what has been previously
      * saved on the stack
      *)
     prelude, pop (n - 1) stack, []

  | CmdCheckSat ->
     let filename = new_file_name () in
     printf "Outputting problem %s@." filename;
     let oc = open_out_bin filename in
     let ppf = formatter_of_out_channel oc in
     let mycmds = List.rev (exit_cmd :: (stack_script ((cmd :: cmds) :: stack) @ prelude)) in
     Pp.pp_commands ppf mycmds;
     close_out oc;
     prelude, stack, cmds

  | CmdSetLogic _
  | CmdSetOption _
  | CmdSetInfo _ -> (cmd :: prelude), stack, cmds
  | CmdDefineFun _
  | CmdDeclareFun _
  | CmdDefineSort _
  | CmdDeclareSort _
  | CmdDeclareConst _
  | CmdDefineFunRec _
  | CmdGetModel
  | CmdGetUnsatAssumptions
  | CmdMetaInfo _
  | CmdReset
  | CmdResetAssertions
  | CmdGetInfo _
  | CmdGetValue _
  | CmdGetProof
  | CmdGetOption _
  | CmdGetUnsatCore
  | CmdGetAssignment
  | CmdGetAssertions
  | CmdAssert _
  | CmdEcho _
  | CmdCheckSatAssuming _
  | CmdExit -> prelude, stack, (cmd :: cmds)
;;

let apply script =
  let _filename = script.script_loc.loc_start.pos_fname in
  (* set_basename filename; *)
  ignore(List.fold_left eval ([], empty_assertion_set, []) script.script_commands)
;;
