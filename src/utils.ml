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

module StringMap =
  Map.Make(
      struct
        type t = string
        let compare = String.compare
      end
)
;;

module StringSet = struct
  include Set.Make(
              struct
                type t = string
                let compare = String.compare
              end );;

 let to_list set = fold (fun v l -> v :: l) set []
end
;;

module SymbolMap =
  Map.Make(
      struct
        open Ast ;;
        type t = symbol ;;
        let compare s1 s2 =
          Pervasives.compare s1.symbol_desc s2.symbol_desc ;;
      end
    )
;;

module SymbolSet =
  Set.Make(
      struct
        open Ast ;;
        type t = symbol ;;
        let compare s1 s2 =
          Pervasives.compare s1.symbol_desc s2.symbol_desc ;;
      end
    )
;;


let mk_header fmt s =
  let slen = String.length s in
  let sub_hdr = String.make slen '=' in
  Format.fprintf fmt "@[<v 0>%s@ %s@ @]" s sub_hdr
;;


let sfprintf fmt =
  let b = Buffer.create 20 in
  let return fmt = Format.pp_print_flush fmt (); Buffer.contents b in
  Format.kfprintf return (Format.formatter_of_buffer b) fmt
;;

let default_opt v opt =
  match opt with
  | None -> v
  | Some v' -> v'
;;

let third (_, _, z) = z ;;

(* Extract the substring from [beg_idx] to [end_idx included from [s] *)
let string_extract s beg_idx end_idx =
  String.sub s beg_idx (end_idx - beg_idx + 1)
;;

(* Make a list of all substrings from [s] separated by character [c] *)
let string_explode c s =
  let len = String.length s in
  let rec aux last_idx l =
    try
      if last_idx >= len then List.rev l
      else
        begin
          let idx = String.index_from s last_idx c in
          if idx = last_idx then aux (idx + 1) l
          else
            let s = string_extract s last_idx (idx - 1) in
            let new_list = s :: l in
            aux (idx + 1) new_list
        end
    with
      | Not_found ->
        List.rev ((string_extract s last_idx (len - 1)) :: l)
  in aux 0 []
;;


let rec has_more_than (n : int) (f : 'a -> bool) (l : 'a list) =
  n < 0 ||
    match l with
    | [] -> false
    | h :: tl ->
       let m = if f h then pred n else n in
       has_more_than m f tl
;;
