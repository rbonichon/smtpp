(*********************************************************************************)
(*  Copyright (c) 2015, INRIA, Universite de Nancy 2 and Universidade Federal    *)
(*  do Rio Grande do Norte.                                                      *)
(*                                                                               *)
(*  Permission to use, copy, modify, and distribute this software for any        *)
(*  purpose with or without fee is hereby granted, provided that the above       *)
(*  copyright notice and this permission notice appear in all copies.            *)
(*                                                                               *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES     *)
(*  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF             *)
(*  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR      *)
(*  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES       *)
(*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN        *)
(*  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF      *)
(*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.               *)
(*********************************************************************************)

(** Generate a pair of functions to set and get a boolean switch.
 ** Used by the Config module for boolean command line switches.
*)
let genr_bool_switch () =
  let flag = ref false in
  (fun (b:bool) -> flag := b),
  (fun () -> !flag)
;;

let set_debug, get_debug = genr_bool_switch () ;;

let add_file, get_files, set_file, get_file, clear_files  =
  let (files : string list ref) = ref ([] : string list) in
  (fun (f : string) -> files := f :: !files),
  (fun () -> !files),
  (fun f -> files := [f]),
  (fun () -> List.hd !files),
  (fun () ->  files := [])
;;


let set_pushpop, get_pushpop = genr_bool_switch () ;;

let set_smtsuccess, get_smtsuccess = genr_bool_switch () ;;
(* set_smtsuccess false ;; (* Enable success printing by default *)*)

let set_reprint, get_reprint = genr_bool_switch () ;;

let set_preLA, get_preLA = genr_bool_switch () ;;

let set_preprocessor, get_preprocessor = genr_bool_switch () ;;

let set_obfuscate, get_obfuscate = genr_bool_switch () ;;

let set_detect, get_detect = genr_bool_switch () ;;

let set_unused, get_unused = genr_bool_switch () ;;

let set_rm_unused, get_rm_unused = genr_bool_switch () ;;

let set_keep_symbols, get_keep_symbols =
  let ks = ref [] in
  (fun (s:string) ->
   ks := List.map String.trim (Utils.string_explode ',' s);
  ),
  (fun () -> !ks)
;;

let pp_version () = Format.printf "%s@." Version.version ;;
