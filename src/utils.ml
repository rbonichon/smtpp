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
              end )
end
;;


let mk_header fmt s =
  let slen = String.length s in
  let sub_hdr = String.make slen '=' in
  Format.fprintf fmt "@[<v 0>%s@ %s@ @]" s sub_hdr
;;

let debug s =
  if Config.get_debug () then
    Format.printf "@[<hov 1>[debug] %s@]@." s
;;

let sfprintf fmt =
  let b = Buffer.create 20 in
  let return fmt = Format.pp_print_flush fmt (); Buffer.contents b in
  Format.kfprintf return (Format.formatter_of_buffer b) fmt
;;

let third (_, _, z) = z ;;
