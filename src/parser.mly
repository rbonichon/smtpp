/**************************************************************************/
/*  Copyright (c) 2015 Richard Bonichon <richard.bonichon@gmail.com>      */
/*                                                                        */
/*  Permission to use, copy, modify, and distribute this software for any  */
/*  purpose with or without fee is hereby granted, provided that the above  */
/*  copyright notice and this permission notice appear in all copies.     */
/*                                                                        */
/*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/*  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF      */
/*  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  */
/*  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES  */
/*  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN  */
/*  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  */
/*  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.        */
/*                                                                        */
/**************************************************************************/

%{
    open Ast
    open Locations ;;

    (* Localizing a symbol *)
    let symbol_rloc () = {
        loc_start = Parsing.symbol_start_pos ();
        loc_end = Parsing.symbol_end_pos ();
      }
    ;;

    (* Helper construction functions. Automated marking of file locations *)
    let mk_sexpr sexpr_desc = { sexpr_desc; sexpr_loc = symbol_rloc (); } ;;
    let mk_identifier id_desc = { id_desc; id_loc = symbol_rloc (); } ;;

    let mk_sort sort_desc = { sort_desc; sort_loc = symbol_rloc (); } ;;
    let mk_command command_desc =
      { command_desc; command_loc = symbol_rloc (); }
    ;;

    let mk_fun_def fun_def_desc =
      { fun_def_desc; fun_def_loc = symbol_rloc (); }
    ;;

    let mk_fun_rec_def fun_rec_def_desc =
      { fun_rec_def_desc; fun_rec_def_loc = symbol_rloc (); }
    ;;

    let mk_sorted_var sorted_var_desc =
      { sorted_var_desc; sorted_var_loc = symbol_rloc (); }
    ;;

    let mk_qual_identifier qual_identifier_desc =
      { qual_identifier_desc; qual_identifier_loc = symbol_rloc (); }
    ;;

    let mk_var_binding var_binding_desc =
      { var_binding_desc; var_binding_loc = symbol_rloc (); }
    ;;

    let mk_term term_desc = { term_desc; term_loc = symbol_rloc (); } ;;

    let mk_smt_option smt_option_desc = {
        smt_option_desc; smt_option_loc = symbol_rloc ();
      }
    ;;

    let mk_script script_commands =
      { script_commands; script_loc = symbol_rloc () }
    ;;

    let mk_attribute attribute_desc =
      { attribute_desc; attribute_loc = symbol_rloc (); }
    ;;

    let mk_attr_value attr_value_desc =
      { attr_value_desc; attr_value_loc = symbol_rloc (); }
    ;;

    let mk_info_flag info_flag_desc =
      { info_flag_desc; info_flag_loc = symbol_rloc (); }
    ;;

    let mk_symbol symbol_desc = { symbol_desc; symbol_loc = symbol_rloc (); } ;;
%}

%start script

/* general */
%token BANG
%token UNDERSCORE
%token AS
%token EXISTS
%token FORALL
%token LET

/* commands */
%token SETLOGIC
%token SETOPTION
%token SETINFO
%token DECLARESORT
%token DEFINESORT
%token DECLAREFUN
%token DECLARECONST
%token DEFINEFUN
%token DEFINEFUNREC
%token PAR
/* %token LAMBDA */
%token PUSH
%token POP
%token ASSERT
%token CHECKSAT
%token GETASSERTIONS
%token GETPROOF
%token GETUNSATCORE
%token GETVALUE
%token GETASSIGNMENT
%token GETUNSATASSUMPTIONS
%token GETOPTION
%token GETINFO
%token GETMODEL
%token EXIT
%token ECHO
%token RESET
%token RESETASSERTIONS
%token METAINFO

/* Other tokens */
%token LPAREN
%token RPAREN
%token EOF

%token <Ast.numeral> NUMERAL
%token <string> DECIMAL
%token <string> HEXADECIMAL
%token <string> BINARY
%token <string> STRING
%token <bool>   BOOL
%token <string> KEYWORD
%token <string> SYMBOL
%token <string> QUOTEDSYMBOL

%type  <Ast.script> script

%%

script:
| commands=delimited(LPAREN,command,RPAREN)*; EOF { mk_script commands }
;

%inline command:
| ASSERT t=term;
  { mk_command (CmdAssert t) }
| CHECKSAT
    { mk_command CmdCheckSat }
| DECLARECONST symb=symbol; so=sort;
  { mk_command (CmdDeclareConst(symb, so)) }
| DECLAREFUN symb=symbol; polys=option(poly_parameters); LPAREN sorts=sort* RPAREN so=sort;
  { mk_command (CmdDeclareFun (symb, polys, sorts, so)) }
| DECLARESORT symb=symbol; num=NUMERAL;
  { mk_command (CmdDeclareSort(symb, num)) }
| DEFINEFUN LPAREN fdef=fun_nonrec_def; RPAREN
 { mk_command (CmdDefineFun fdef) }
| DEFINEFUNREC LPAREN frdefs=fun_rec_def+; RPAREN
 { mk_command (CmdDefineFunRec frdefs) }
| DEFINESORT symb=symbol; LPAREN symbs=symbol+ RPAREN so=sort;
  { mk_command (CmdDefineSort (symb, symbs, so)) }
| ECHO s=STRING;
  { mk_command (CmdEcho s) }
| EXIT
  { mk_command CmdExit }
| GETASSERTIONS
    { mk_command CmdGetAssertions }
| GETASSIGNMENT
    { mk_command CmdGetAssignment }
| GETINFO iflag=info_flag;
  { mk_command (CmdGetInfo iflag) }
| GETMODEL
    { mk_command CmdGetModel }
| GETOPTION kwd=KEYWORD;
  { mk_command (CmdGetOption kwd) }
| RESET
    { mk_command CmdReset }
| RESETASSERTIONS
    { mk_command CmdResetAssertions }
| GETPROOF
    { mk_command CmdGetProof }
| GETUNSATCORE
    { mk_command CmdGetUnsatCore }
| GETUNSATASSUMPTIONS
    { mk_command CmdGetUnsatAssumptions }
| GETVALUE LPAREN ts=term+; RPAREN;
  { mk_command (CmdGetValue ts) }
| METAINFO attr=attribute;
  { mk_command (CmdMetaInfo attr) }
| POP num=option(NUMERAL);
  { mk_command (CmdPop num) }
| PUSH num=option(NUMERAL);
  { mk_command (CmdPush num) }
| SETINFO attr=attribute;
  { mk_command (CmdSetInfo attr) }
| SETLOGIC symb=symbol;
  { mk_command (CmdSetLogic symb) }
| SETOPTION sopt=smt_option;
  { mk_command (CmdSetOption sopt) }
;

fun_def:
| symb=symbol; polys=option(poly_parameters); LPAREN svars=sorted_var*; RPAREN so=sort; t=term;
  { (symb, polys, svars, so, t) }
  ;

fun_nonrec_def:
| fd=fun_def;
  { let s, ps, svs, so, t = fd  in mk_fun_def (FunDef (s, ps, svs, so, t)) }
;

fun_rec_def:
| LPAREN fd=fun_def RPAREN
 { let s, ps, svs, so, t = fd in mk_fun_rec_def (FunRecDef (s, ps, svs, so, t)) }
 ;

poly_parameters:
| PAR LPAREN sorts=sort+; RPAREN { sorts }
;

sorted_var:
| LPAREN symb=symbol; so=sort; RPAREN
 { mk_sorted_var (SortedVar (symb, so)) }
  ;

sort:
| id=identifier; { mk_sort (SortIdentifier id) }
| LPAREN id=identifier; sorts=sort+; RPAREN { mk_sort (SortFun (id, sorts)) }
;

index:
| NUMERAL { IdxNum $1 }
| SYMBOL  { IdxSymbol (mk_symbol (SimpleSymbol $1)) }
;

identifier:
| symb=symbol { mk_identifier (IdSymbol symb) }
| LPAREN UNDERSCORE symb=symbol; indexes=index+; RPAREN
  { mk_identifier (IdUnderscore (symb, indexes)) }
;

symbol:
| SYMBOL { mk_symbol (SimpleSymbol $1) }
| QUOTEDSYMBOL { mk_symbol (QuotedSymbol $1) }
;

term:
| spec_constant
 { mk_term (TermSpecConstant $1) }
| qual_identifier
 { mk_term (TermQualIdentifier $1) }
| LPAREN qualid=qual_identifier; ts=term+; RPAREN
 { mk_term (TermQualIdentifierTerms(qualid, ts)) }
| LPAREN LET LPAREN vbindings=var_binding+; RPAREN t=term; RPAREN
 { mk_term (TermLetTerm (vbindings, t)) }
| LPAREN FORALL LPAREN svars=sorted_var+; RPAREN t=term; RPAREN
 { mk_term (TermForallTerm (svars, t)) }
| LPAREN EXISTS  LPAREN svars=sorted_var+; RPAREN t=term; RPAREN
 { mk_term (TermExistsTerm (svars, t)) }
| LPAREN BANG t=term; attrs=attribute+ RPAREN
 { mk_term (TermAnnotatedTerm(t, attrs)) }
;

qual_identifier:
| id=identifier;
  { mk_qual_identifier (QualIdentifierIdentifier id) }
| LPAREN AS id=identifier; so=sort; RPAREN
 { mk_qual_identifier (QualIdentifierAs (id, so)) }
  ;

var_binding:
| LPAREN symb=symbol; t=term; RPAREN
 { mk_var_binding (VarBinding (symb, t)) }
  ;

spec_constant:
| BINARY  { CstBinary $1 }
| DECIMAL { CstDecimal $1 }
| NUMERAL { CstNumeral $1 }
| STRING  { CstString $1 }
| BOOL    { CstBool $1 }
| HEXADECIMAL { CstHexadecimal $1 }
;

attribute_value:
| sc=spec_constant;
  { mk_attr_value (AttrValSpecConstant sc) }
| symb=symbol;
  { mk_attr_value (AttrValSymbol symb) }
| LPAREN sexprs=sexpr*; RPAREN
 { mk_attr_value (AttrValSexpr sexprs) }
;

sexpr:
| sc=spec_constant;
  { mk_sexpr (SexprConstant sc) }
| symb=symbol;
  { mk_sexpr (SexprSymbol symb) }
| kwd=KEYWORD;
  { mk_sexpr (SexprKeyword kwd) }
| LPAREN sexprs=sexpr*; RPAREN
 { mk_sexpr (SexprParens sexprs) }
;

attribute:
| kwd=KEYWORD;
  { mk_attribute (AttrKeyword kwd) }
| kwd=KEYWORD; attrval=attribute_value;
  { mk_attribute (AttrKeywordValue (kwd, attrval)) }
;

smt_option:
| attr=attribute { mk_smt_option (OptionAttribute attr) }
;

info_flag:
| kwd=KEYWORD { mk_info_flag (InfoFlagKeyword kwd) }
