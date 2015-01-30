(** Generate a pair of functions to set and get a boolean switch.
 ** Used by the Config module for boolean command line switches.
*)
let genr_bool_switch () =
  let flag = ref false in
  (fun (b:bool) -> flag := b),
  (fun () -> !flag)
;;

let set_debug, get_debug = genr_bool_switch () ;;

let set_file, get_file =
  let file = ref "-" in
  (fun s ->
   if !file = "-" then file := s),
  (fun () -> !file)
;;

let set_pushpop, get_pushpop = genr_bool_switch () ;;

let set_smtsuccess, get_smtsuccess = genr_bool_switch () ;;
