(* Parsing utilities to extract information from models produced by supported
 * SMT-Provers:
 * - Z3
 * - Boolector
 * - CVC4
 * - Yices
 *)


module Int64Hashtbl =
  Hashtbl.Make(
  struct
    type t = int64
    let equal x y = Int64.compare x y = 0
    let hash = Hashtbl.hash
  end
  )

module StringHashtbl =
  Hashtbl.Make(
  struct
    type t = string
    let equal x y = String.compare x y = 0
    let hash = Hashtbl.hash
  end
  )

type t = {
  memory : int Int64Hashtbl.t;
  registers : Int64.t StringHashtbl.t;
}

let create () =
  { memory = Int64Hashtbl.create 7;
    registers = StringHashtbl.create 43;
  }

let add_var t name bv =
  StringHashtbl.replace t.registers name bv

let add_memcell t address value =
  Int64Hashtbl.replace t.memory address value

let pp ppf t =
  let open Format in
  let pp_registers ppf () =
    StringHashtbl.iter
      (fun name bv ->
         fprintf ppf "%s : %Lx@ "
           name bv 
      ) t.registers
  and pp_memory ppf () =
    Int64Hashtbl.iter
      (fun address value -> fprintf ppf "%Lx : %x@ " address value ) t.memory
  in
  fprintf ppf
    "@[<v 2>\
     -- Model --@ \
     # Memory@ \
     %a@ \
     # Registers@ \
     %a@ \
     @]"
    pp_memory ()
    pp_registers ()
    

(* For SMT-LIB compliant provers 
 * The generic SMT-LIB model format is defined in ??
*)



(* 2 pass algorithm to construct a model which has values for registers and for
 * some memory addresses.
 *   1. Extract all information related to named constants (i.e. functions of
 *      arity 0 in SMT-LIB): this represents values of registers at various
 *      steps. 
 *
 *   2. Extract from the remaining functions the contents of the memory
 *      (i.e. arbitrary addresses). The way provers do that does not seem to be
 *      standardized. We will have different techniques for different provers.
*)

open Ast

let get_symbol_name symbol =
  match symbol.symbol_desc with
  | SimpleSymbol str 
  | QuotedSymbol str -> str


let value_of_constant cst =
  match cst with
  | CstHexadecimal s -> Int64.of_string ("0x" ^ s)
  | CstBinary s -> Int64.of_string ("0b" ^ s)
  | CstNumeral _
  | CstString _
  | CstBool _
  | CstDecimal _ ->
    Io.error
      "Model construction: unexpected constant %a as bitvector value"
      Pp.pp_spec_constant cst;
    exit 2

let value_of_index converter = function
  | IdxNum num ->
    Io.debug "idx: %s@." num;
    Some (converter num)
  | IdxSymbol _ -> None


let starts_with_bv name = name.[0] = 'b' && name.[1] = 'v'
                                            
let bv_from_qid qid =
  match qid.qual_identifier_desc with
  | QualIdentifierIdentifier id ->
    begin
      match id.id_desc with
      | IdSymbol _ -> assert false
      | IdUnderscore(symbol, indexes) ->
        let name = get_symbol_name symbol in
        assert (starts_with_bv name);
        assert (List.length indexes = 1);
        Int64.of_string (String.sub name 2 (String.length name - 2))
    end
  | QualIdentifierAs _ -> assert false
    
(* Bitvector values are encoded in at various ways in models
*  - bv<decimal_value> : found in CVC4, handled by [bv_from_qid]
*  - #x<hexa>
*  - #b<binary>
*)
let extract_bitvector term =
  match term.term_desc with
  | TermSpecConstant cst -> value_of_constant cst
  | TermQualIdentifier qid -> bv_from_qid qid
  | TermAnnotatedTerm _
  | TermExistsTerm _
  | TermForallTerm _
  | TermLetTerm _
  | TermQualIdentifierTerms _  -> assert false
    
let get_bitvector_size sort =
  match sort.sort_desc with
  | SortIdentifier identifier ->
    begin
      match identifier.id_desc with
      | IdSymbol _ -> None
      | IdUnderscore (symbol, indexes) ->
        if String.compare (get_symbol_name symbol) "BitVec" = 0 then
          match indexes with
          | [idx] -> value_of_index int_of_string idx 
          | [] | _ :: _ -> None
        else None
    end
  | SortFun _ -> None

let add_variable smt_model cmd =
  match cmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (symbol, _, _, sort, term) ->
        let size =
          match get_bitvector_size sort with
          | Some sz -> sz
          | None -> 0
        in
        let bv = extract_bitvector term in
        let name = get_symbol_name symbol in
        Io.debug "Add register %s as bv %Lx (%L)@." name bv size;
        add_var smt_model name bv
    end
  | _ -> assert false

let is_bitvector sort = get_bitvector_size sort <> None


let sort_of_sorted_var svar =
  match svar.sorted_var_desc with
  | SortedVar (_, sort) -> sort
    

let is_bv_bv_function args sort =
  match args with
  | [arg] ->
    let arg_sort = sort_of_sorted_var arg in
    is_bitvector arg_sort && is_bitvector sort
  | [] | _ -> false 


let is_bv_bv_array sort =
  match sort.sort_desc with
  | SortIdentifier _ -> false
  | SortFun (id, sorts) ->
    match id.id_desc, sorts with
    | IdUnderscore _, _ -> false
    | IdSymbol symbol, s1 :: [s2] ->
      if String.compare (get_symbol_name symbol) "Array" = 0 then
        is_bitvector s1 && is_bitvector s2
      else false
    | IdSymbol _, ([] | _ :: _) -> false


let is_bv_variable cmd =
  match cmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (_, optargs, args, sort, _) ->
        optargs = None && args = [] && is_bitvector sort
    end
  | _ -> assert false

          
let is_memory funcmd =
  match funcmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (_, _, args, sort, _) ->
        is_bv_bv_array sort
        ||
        is_bv_bv_function args sort
    end
  | _ -> false


let is_string s qid =
  match qid.qual_identifier_desc with
  | QualIdentifierIdentifier id ->
    begin
      match id.id_desc with
      | IdSymbol symb -> String.compare (get_symbol_name symb) s = 0
      | IdUnderscore _ -> false
    end
  | QualIdentifierAs _ -> false

let is_ite = is_string "ite"
let is_eq = is_string "="
let is_store = is_string "store"

let extract_address eqterm =
  match eqterm.term_desc with
  | TermQualIdentifierTerms (qid, terms) ->
    begin
      assert (is_eq qid);
      match terms with
      | _vname :: [tcst] ->
        begin
          match tcst.term_desc with
          | TermSpecConstant c -> value_of_constant c
          | _ -> assert false
        end
      | _ -> assert false
    end
  | TermSpecConstant _ 
  | TermLetTerm _
  | TermForallTerm _
  | TermExistsTerm _
  | TermAnnotatedTerm _
  | TermQualIdentifier _ -> assert false


let extract_byteval tbyteval =
  match tbyteval.term_desc with
  | TermSpecConstant c -> value_of_constant c
  | TermQualIdentifierTerms _
  | TermLetTerm _
  | TermForallTerm _
  | TermExistsTerm _
  | TermAnnotatedTerm _
  | TermQualIdentifier _ -> assert false

    
(* The memory is represented by a chain of if-then-else *)
let memory_from_ite smt_model term =
  let rec extract_from term =
    match term.term_desc with
    | TermQualIdentifierTerms (qid, terms) ->
      (* This a term of the form (ite (= X v) v2 t) *)
      assert (is_ite qid);
      begin
        match terms with
        | teqcond :: tbyteval :: [term] ->
          let address = extract_address teqcond in
          let byteval = extract_byteval tbyteval in
          add_memcell smt_model address (Int64.to_int byteval);
          extract_from term
        | _ -> assert false
      end
    | TermSpecConstant _ ->
       (* Last default case : ignored. Usually #x00 *)
      ()
    | TermLetTerm _
    | TermForallTerm _
    | TermExistsTerm _
    | TermAnnotatedTerm _
    | TermQualIdentifier _ -> assert false
  in extract_from term



let memory_from_store smt_model term =
  let rec extract_from term =
    match term.term_desc with
    | TermQualIdentifierTerms (qid, terms) ->
      begin
        if is_store qid then
          match terms with
          | prev_store :: taddr :: [tbyteval] ->
            let addr = extract_bitvector taddr in
            let byte = extract_bitvector tbyteval in
            add_memcell smt_model addr (Int64.to_int byte);
            extract_from prev_store
          | _ ->  assert false
      end
    | _ -> assert false
  in extract_from term


let is_ite_memory term =
  match term.term_desc with
  | TermQualIdentifierTerms (qid, _) -> is_ite qid
  | _ -> false

let is_store_memory term =
  match term.term_desc with
  | TermQualIdentifierTerms (qid, _) -> is_string "store" qid
  | _ -> false

let get_function_body fcmd =
  match fcmd.command_desc with
  | CmdDefineFun fdef ->
    begin
      match fdef.fun_def_desc with
      | FunDef (_, _, _, _, term) -> term
    end
  | _ -> assert false
    
(* Memory is defined as an array of addresses to bytes.
 * In SMT2 parlance this means BitVec 32 -> BitVec 8
 * The memory function is declared in the initial SMT-LIB Script but
 * it is not always present as such in the result
 *  - CVC4 :: it is.
    - Z3 :: there is an indirection to another Array,
 *  - Boolector :: the name is absent
*) 
let find_and_add_memory smt_model functions =
  match List.filter is_memory functions with
  | [] -> Io.warning "No memory found in SMT model@."
  | [f] -> 
    (* Case of Boolector and CVC4 *)
    let body = get_function_body f in
    if is_ite_memory body then memory_from_ite smt_model body
    else if is_store_memory body then memory_from_store smt_model body
    else Io.warning "Could not find appropriate extractor for this memory@."
  | f :: [g] ->
    let fbody = get_function_body f in
    if is_ite_memory fbody then memory_from_ite smt_model fbody
    else
      let gbody = get_function_body g in
      if is_ite_memory gbody then memory_from_ite smt_model gbody
    else Io.warning "Unexpected memory encoding from SMT model (Looked like Z3)@."
  | _ ->
    Io.warning "Unexpected memory encoding from SMT model@.";
    ()

let extract model_ast =
  let smt_model = create () in
  let variables, functions =
    List.partition is_bv_variable model_ast.model_commands in
  Io.debug "Found : %d variables, %d functions@."
    (List.length variables) (List.length functions);
  List.iter (add_variable smt_model) variables;
  Io.debug "%d functions are potential memories@."
    (List.length (List.filter is_memory functions));
  find_and_add_memory smt_model functions;
  smt_model



module Yices = struct

  let is_equal = is_string "="
  let is_function = is_string "function"

  let size_of_bv_constant term = 1

  let name_of_identifier id =
    match id.id_desc with
    | IdSymbol symb -> get_symbol_name symb
    | IdUnderscore _ -> assert false
      
  let name_of_qid qid =
    match qid.qual_identifier_desc with
    | QualIdentifierIdentifier id -> name_of_identifier id
    | QualIdentifierAs _ -> assert false

  let name_of_qid_term term =
    match term.term_desc with
    | TermQualIdentifier qid -> name_of_qid qid
    | _ -> assert false
            

  let get_address term =
    match term.term_desc with
    | TermQualIdentifierTerms (_qualid, terms) ->
      begin
        match terms with
        | [tvalue] -> extract_bitvector tvalue
        | _ -> assert false
      end
    | _ -> assert false
  (* (function @fun_49
 (type (-> (bitvector 32) (bitvector 8)))
 (= (@fun_49 #b00000001000000000100111101100000) #b10100011)
 (= (@fun_49 #b00000001000000000100111101100001) #b11011010)
 (= (@fun_49 #b00000001000000000100111101100010) #b11111100)
 (= (@fun_49 #b00000001000000000100111101100011) #b01110101)
 (= (@fun_49 #b00000001000000000100111101011100) #b01000100)
 (= (@fun_49 #b00000001000000000100111101011101) #b11001110)
 (= (@fun_49 #b00000001000000000100111101011110) #b11111100)
 (= (@fun_49 #b00000001000000000100111101011111) #b01110101)
 (= (@fun_49 #b00000001000000000100010101111111) #b10000000)
 (default #b00000000))
  *)
  let add_memory model terms =
    let add_cell term =
      match term.term_desc with
      | TermQualIdentifierTerms(qualid, terms) ->
        begin
          if is_equal qualid then
            match terms with
            | taddress :: [tvalue] ->
              let address = get_address taddress in
              let value = extract_bitvector tvalue in
              add_memcell model address (Int64.to_int value)
          else if is_string "default" qualid then Format.printf "DONE"
        end
      | _ -> assert false
    in
    match terms with
    | _name :: _type :: terms -> List.iter add_cell terms
    | [] | [_] -> assert false
  
  let handle_term model term =
    match term.term_desc with
    | TermQualIdentifierTerms (qualid, terms) ->
      Io.debug "%a@." Pp.pp_term term;
      if is_equal qualid then
        (* Register case *)
        match terms with
        | tname :: [tvalue] ->
          let name = name_of_qid_term tname in
          if String.compare name "memory" <> 0 then 
          let value = extract_bitvector tvalue in
          let size = size_of_bv_constant tvalue in
          add_var model name value 
        | [] | _ :: _ -> assert false
      else
      if is_function qualid then add_memory model terms;
        
    | TermExistsTerm _
    | TermAnnotatedTerm _
    | TermForallTerm _
    | TermLetTerm _
    | TermQualIdentifier _
    | TermSpecConstant _ -> assert false
  
  (* Yices has a special term based model. What is identified is the following:
   - named constants are terms of the form (= name value)
     - the memory is a function from bitvector to bitvectors
  *)
let extract terms =
  let smt_model = create () in
  List.iter (handle_term smt_model) terms;
  smt_model
end




