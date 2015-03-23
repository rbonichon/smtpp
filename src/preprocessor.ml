open Ast ;;
open Format;;
(* Pre-processing of predefined operators w.r.t. the theories of declared logics*)

module PredefinedOp = struct
    type predefined_op =
      PredefinedOp of Ast.symbol * Ast.sorts option * Ast.sorts * Ast.sort ;;

    let equal_symbs sy1 sy2 =
      match sy1.symbol_desc, sy2.symbol_desc with
      | SimpleSymbol s1, SimpleSymbol s2 ->
         String.compare s1 s2 = 0;
      | _, _ -> false

    let equal_index i1 i2 =
      match i1, i2 with
      | IdxNum n1, IdxNum n2 -> (compare n1 n2 = 0)
      | IdxSymbol sy1, IdxSymbol sy2 -> (equal_symbs sy1 sy2)
      | _, _ -> false

    (*TODO: rewrite it using foldleft. Is it possible?
     * List.fold_left (fun result i1 i2 -> result && (equal_index i1 i2)) true
     * *)
    let rec equal_indexes i1 i2 =
      match i1, i2 with
      | [], [] -> true
      | x1::xs1, x2 :: xs2 -> (equal_index x1 x2) && (equal_indexes xs1 xs2)
      | _, _ -> false

    let equal_ids id1 id2 =
      match id1.id_desc, id2.id_desc with
      | IdSymbol sy1, IdSymbol sy2 -> (equal_symbs sy1 sy2)
      | IdUnderscore(sy1, i1), IdUnderscore(sy2, i2) ->
         (equal_symbs sy1 sy2) && (equal_indexes i1 i2)
      | _, _ -> false

    let rec equal_sorts par_s1 par_s2 =
      match par_s1, par_s2 with
      | [], [] -> true
      | x1 :: xs1, x2 :: xs2 ->
          (equal_sort x1 x2) && (equal_sorts xs1 xs2)
      | _, _ -> false

    and equal_sort ps1 ps2 =
      match ps1.sort_desc, ps2.sort_desc with
      | SortIdentifier id1, SortIdentifier id2 -> (equal_ids id1 id2)
      | SortFun(id1, s1), SortFun(id2, s2) ->
          (equal_ids id1 id2) && (equal_sorts s1 s2)
      | _, _ -> false


    let equal_predefined_ops op1 op2 =
      match op1, op2 with
      | PredefinedOp(sy1, Some par_s1, dom1, codom1),
        PredefinedOp(sy2, Some par_s2, dom2, codom2) ->
        (equal_symbs sy1 sy2) && (equal_sorts par_s1 par_s2)
        && (equal_sorts dom1 dom2) && (equal_sort codom1 codom2)
      | PredefinedOp(sy1, None, dom1, codom1),
        PredefinedOp(sy2, None, dom2, codom2) ->
        (equal_symbs sy1 sy2) &&
          (equal_sorts dom1 dom2) && (equal_sort codom1 codom2)

  end
;;

(* Dummy placeholder *)
let preprocess_logic _ = Ast.CmdExit ;;

let preprocess_command cmd =
  let command =
    match cmd.command_desc  with
    | CmdSetLogic logic -> preprocess_logic logic
  in { cmd with command_desc = command }
;;

let preprocess_commands cmds = List.map preprocess_command cmds ;;

let get_hashtable = (fun () ->  ())

  let apply script =
  let script_commands = preprocess_commands script.script_commands in
  let preprocessed_script = { script with script_commands } in
  printf "%a" Pp.pp preprocessed_script;
  (* if Config.get_debug () then
   *   printf "@[<v 0>%a@ %a@]"
   *          Utils.mk_header "Symbol table"
   *          (fun fmt h ->
   *           SymbHash.iter
   *          (fun k v ->
   *           Format.fprintf fmt "%a -> %a@ " Pp.pp_symbol k Pp.pp_symbol v)
   *            h) (get_hashtable ()); *)
;;
