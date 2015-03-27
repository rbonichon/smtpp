type sort_identifier = string ;;
type arity = int ;;

type t =
  | Basic of sort_identifier * arity
  | App of t * t list
  | Fun of t list * t
  | Var of string
  | Poly of string list * t
;;


let int_sort = Basic("Int", 0) ;;
let bool_sort = Basic("Bool", 0) ;;
let real_sort = Basic("Real", 0) ;;
let array_sort = Poly(["X"], App(Basic("Array", 1), [Var "X";])) ;;

let xx_y_fun x y = Fun([x; x;], y) ;;
let x_y_fun x y = Fun([x; ], y) ;;


let basic_sort_name (sort : t) =
  match sort with
  | Basic (sid, _) -> sid
  | _ -> assert false
;;


module SortSet =
  Set.Make(struct
              type t;;
              let compare = Pervasives.compare ;;
            end)
;;
