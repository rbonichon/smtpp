open Format ;;

type arith_sort = Integer | Real | Mixed ;;
type arith_kind = Difference | Linear | NonLinear ;;
(* Kind of arithmetics used in SMT-LIB benchmarks.
   They form a simple lattice.
*)

let arith_kind_compare (k1 : arith_kind) (k2 : arith_kind) : int =
  match k1, k2 with
  | Difference, Difference
  | Linear, Linear
  | NonLinear, NonLinear -> 0
  | NonLinear, (Difference | Linear) -> 1
  | (Difference | Linear), NonLinear -> -1
  | Linear, Difference -> 1
  | Difference, Linear -> -1
;;

let max_kind_opt (k1 : arith_kind option) (k2 : arith_kind option)
    : arith_kind option =
  match k1, k2 with
  | Some _, None -> k1
  | None, Some _ -> k2
  | Some x, Some y -> if arith_kind_compare x y > 0 then k1 else k2
  | None, None -> None
;;

let arith_sort_compare (s1 : arith_sort) (s2 : arith_sort) : int =
  match s1, s2 with
  | Real, Real
  | Integer, Integer
  | Mixed, Mixed -> 0
  | Mixed, (Real | Integer) -> 2
  | (Real | Integer), Mixed -> -2
  | Real, Integer -> 1
  | Integer, Real -> -1
;;

type t = {
    smt_name : string;
    mutable array : bool ; (* Array theory on / off ? *)
    mutable uninterpreted_functions : bool ;
    mutable bitvectors : bool ;
    mutable quantifiers : bool ;
    mutable arithmetic_sort : arith_sort option ;
    mutable arithmetic_kind : arith_kind option ;
  }
;;

let default = {
    smt_name = "UNKNOWN";
    array = false;
    uninterpreted_functions = false;
    bitvectors = false;
    quantifiers = true;
    arithmetic_kind = None;
    arithmetic_sort = None;
  }
;;

let parse_logic (smt_logic : string) =
  let logic = { default with smt_name = smt_logic } in
  let idx = ref 0 in
  let lidx = String.length smt_logic - 1 in
  let parse_arithmetic (smt_logic : string) (idx : int ref) =
    (match smt_logic.[!idx] with
     | 'L' -> logic.arithmetic_kind <- Some Linear
     | 'N' -> logic.arithmetic_kind <- Some NonLinear
     | _ -> assert false);
    incr idx;
    if smt_logic.[!idx] = 'I' then
      (logic.arithmetic_sort <- Some Integer; incr idx;);
    if smt_logic.[!idx] = 'R' then begin
      incr idx;
      if logic.arithmetic_sort = None then logic.arithmetic_sort <- Some Real
      else begin
        assert (logic.arithmetic_sort = Some Integer);
        logic.arithmetic_sort <- Some Mixed;
        end;
      end;
    assert (smt_logic.[!idx] = 'A');
    incr idx;
  in
  try
  while !idx <= lidx do
    match smt_logic.[!idx] with
    | 'Q' ->
       begin
         logic.quantifiers <- false;
         assert(!idx + 2 <= lidx  &&
                  smt_logic.[!idx + 1] = 'F' && smt_logic.[!idx + 2] = '_');
         idx := !idx + 3;
       end
    | 'L' | 'N' -> parse_arithmetic smt_logic idx
    | 'A' ->
       begin
         logic.array <- true;
         incr idx;
         (* Sometimes, as in QF_AX, 'X' occurs after 'A' *)
         if !idx <= lidx && smt_logic.[!idx] = 'X' then incr idx;
       end
    | 'U' ->
       logic.uninterpreted_functions <- true;
       (* U is always followed by F *)
       assert (!idx < lidx && smt_logic.[!idx + 1] = 'F');
       idx := !idx + 2;
    | 'B' ->
       logic.bitvectors <- true;
       (* U is always followed by V *)
       assert (!idx < lidx && smt_logic.[!idx + 1] = 'V');
       idx := !idx + 2;
    | 'I' ->
       (* When occurring here, "DL" is appended *)
       logic.arithmetic_sort <- Some Integer ;
       assert(!idx + 2 <= lidx  &&
                smt_logic.[!idx + 1] = 'D' && smt_logic.[!idx + 2] = 'L');
       logic.arithmetic_kind <- Some Difference;
       idx := !idx + 3;
    | 'R' ->
       (* When occurring here, "DL" is appended *)
       logic.arithmetic_sort <- Some Real ;
       assert(!idx + 2 <= lidx  &&
                smt_logic.[!idx + 1] = 'D' && smt_logic.[!idx + 2] = 'L');
       logic.arithmetic_kind <- Some Difference;
       idx := !idx + 3;
    | _ -> assert false
  done;
  assert (!idx = lidx + 1);
  logic
  with
  | e ->
     Format.printf "Error while parsing logic name %s@." smt_logic;
     raise e;
;;


(* List of logics know and defined by SMT-LIB *)
let smt_logics =
  let qf_suffixes =
    [ "AX"; "IDL"; "UF"; "BV"; "RDL"; "LIA"; "UFIDL"; "UFBV"; "LRA"; "NIA";
      "ALIA"; "UFLIA"; "UFLRA"; "UFNRA"; "UFNIA"; "AUFBV"; "AUFLIA";
      "NRA";
    ]
  in
  let qf_logics = List.map (fun sfx -> "QF_"^sfx) qf_suffixes in
  let q_logics = [
      "LIA"; "NIA"; "LRA"; "NRA";
      "ALIA"; "UFNIA";
      "AUFLIA";
      "AUFLIRA";
      "AUFNIRA";
    ]
  in q_logics @ qf_logics
;;

let is_smt_logic (logic : string) = List.mem logic smt_logics;;

let only_array (logic : t) =
  logic.array && not logic.quantifiers && not logic.bitvectors
  && logic.arithmetic_kind = None && logic.arithmetic_sort = None
  && not logic.uninterpreted_functions
;;

let pp_arithmetic fmt (logic : t) =
  match logic.arithmetic_kind with
  | None -> ()
  | Some (Linear | NonLinear as l) ->
     begin
       if l = Linear then fprintf fmt "L" else fprintf fmt "N";
       (match logic.arithmetic_sort with
        | None -> assert false
        | Some Integer -> fprintf fmt "I"
        | Some Real -> fprintf fmt "R"
        | Some Mixed -> fprintf fmt "IR"
       );
       fprintf fmt "A";
     end
  | Some Difference ->
     begin
       (match logic.arithmetic_sort with
        | None -> assert false
        | Some Mixed -> fprintf fmt "IR (?)"
        | Some Integer -> fprintf fmt "I"
        | Some Real -> fprintf fmt "R"
       );
       fprintf fmt "DL";
     end
;;

(* Pretty-prints a logic theory without looking at its name.
 * Useful when inferring a theory from the script if it has not been informed.
 * In this case, we can self-generate the name and insert it.
 * The generated name follows the implicit order of SMT-LIB names.
 *)
let pp_from_core fmt (logic : t) =
  if not logic.quantifiers then fprintf fmt "QF_";
  if logic.array then begin
    fprintf fmt "A";
    if only_array logic then fprintf fmt "X";
    end;
  if logic.uninterpreted_functions then fprintf fmt "UF";
  if logic.bitvectors then fprintf fmt "BV";
  fprintf fmt "%a" pp_arithmetic logic;
;;
