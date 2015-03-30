open Format ;;
open Utils ;;

type sort_identifier = string ;;
type arity = int ;;

type t =
  | Basic of sort_identifier * arity
  | App of t * t list
  | Fun of t list * t
  | Var of string
  | Poly of string list * t
;;

let rec pp_sort fmt (sort : t) =
  match sort with
  | Basic (sort_id, _) -> fprintf fmt "%s" sort_id
  | App (sort, sorts) ->
     fprintf fmt "@[<hov 1>%a@ (%a)@]" pp_sort sort (pp_sorts ",") sorts
  | Fun (sorts, sort) ->
     fprintf fmt "@[<hov 1>(%a) -> %a@]" (pp_sorts "*") sorts pp_sort sort
  | Var vname -> fprintf fmt "%s" vname
  | Poly (pnames, sort) ->
     let rec pp_names fmt =  function
       | [] -> ()
       | [name] -> fprintf fmt "%s" name
       | name :: names -> fprintf fmt "%s, %a" name pp_names names
     in fprintf fmt "\\(%a).%a" pp_names pnames pp_sort sort

and pp_sorts (sep : string) fmt sorts =
  match sorts with
  | [] -> ()
  | [sort] -> pp_sort fmt sort
  | sort :: sorts -> fprintf fmt "%a %s %a" pp_sort sort sep (pp_sorts sep) sorts
;;

let rec occur (sort : t) (name : string) =
  match sort with
  | Basic (sort_id, _) -> String.compare name sort_id = 0
  | App (sort, sorts)
  | Fun (sorts, sort) ->
     let rec in_sorts = function
       | [] -> false
       | sort :: sorts  -> occur sort name || in_sorts sorts
     in occur sort name || in_sorts sorts
  | Var vname -> String.compare name vname = 0
  | Poly (pvars, sort) ->
     (* Do not continue if the name has been bound again *)
     not (List.mem name pvars) && occur sort name
;;

let free_variables (sort : t) =
  let rec aux = function
    | Basic _ -> StringSet.empty
    | Fun (sorts, sort)
    | App (sort, sorts) ->
       let free =
         List.fold_left
           (fun s sort -> StringSet.union s (aux sort)) StringSet.empty sorts
       in StringSet.union free (aux sort)
    | Var vname -> StringSet.singleton vname
    | Poly (pvars, sort) ->
       let free = aux sort in
       StringSet.diff free (StringSet.of_list pvars)
  in aux sort
;;


let mk_var =
  let basename = "V" in
  let num = ref (-1) in
  fun () -> incr num; Var (basename^(string_of_int !num))
;;

let get_var_name (v : t) =
  match v with
  | Var vname -> vname
  | _ -> assert false
;;

let mk_basic (name : string) (arity : int) = Basic (name, arity)

let mk_fun (sorts : t list) (rsort : t) = Fun(sorts, rsort) ;;

let compute_arity (sort : t) =
  let rec aux (applied : int) = function
    | Basic (_, arity) -> arity - applied
    | App (sort, l) -> aux (applied + List.length l) sort
    | Fun (l, _) -> List.length l - applied
    | Var _ -> 0
    | Poly (_, sort) -> aux applied sort
  in aux 0 sort
;;

let mk_app (asort : t) (sorts : t list) =
  let arity_left = compute_arity asort in
  let nparams = List.length sorts in
  if nparams <> arity_left then
    let bmsg =
      if nparams > arity_left then "Too many parameters"
      else "Not enough parameters"
    in
    let msg =
      Utils.sfprintf
      "@[<hov 2>%s: cannot apply sort %a to sorts [%a]@]@."
        bmsg pp_sort asort (pp_sorts ",") sorts
    in raise (Invalid_argument msg)
  else App(asort, sorts)
;;

let mk_poly (names : string list) (sort : t) =
  let bound_names = List.filter (occur sort) names in
  Poly(bound_names, sort)
;;

let generalize (sort : t) =
  let fv = StringSet.to_list (free_variables sort) in
  Poly(fv, sort)
;;

let create_vars (n : int) =
  assert (n > 0);
  let rec aux l n =
    if n = 0 then l
    else aux (mk_var () :: l) (n - 1)
  in aux [] n
;;

let generics (name : string) (arity : int) =
  let l = create_vars arity in
  let vnames = List.map get_var_name l in
  Poly (vnames, mk_app (mk_basic name arity) l)
;;

let int_sort = mk_basic "Int" 0 ;;
let bool_sort = mk_basic "Bool" 0 ;;
let real_sort = mk_basic "Real" 0 ;;
let unit_sort = mk_basic "Unit" 0 ;;
let array_sort = generics "Array" 1 ;;
let bitvector_sort = generics "BitVec" 1 ;;

let xx_y_fun x y = Fun([x; x;], y) ;;
let x_y_fun x y = Fun([x; ], y) ;;

let get_basic_sort_name (sort : t) =
  match sort with
  | Basic (sid, _) -> sid
  | _ -> assert false
;;

let basic_sorts (sort : t) =
  let rec aux (sset : Utils.StringSet.t) = function
    | Basic (bname, _) -> Utils.StringSet.add bname sset
    | App (sort, sorts)
    | Fun (sorts, sort) -> List.fold_left aux (aux sset sort) sorts
    | Var _ -> sset
    | Poly (_, sort) -> aux sset sort
  in aux Utils.StringSet.empty sort
;;

module SortSet =
  Set.Make(struct
              type t;;
              let compare = Pervasives.compare ;;
            end)
;;
