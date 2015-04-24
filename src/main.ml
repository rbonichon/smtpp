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

(* Default message to the user *)
let umsg = "Usage: smtpp <file>";;


(*
 * Specification of the known command-line switches of this program.
 * See OCaml's Arg module.
*)
let rec argspec =
  [
  "--help", Arg.Unit print_usage ,
  " print this option list and exits";
  "-help", Arg.Unit print_usage ,
  " print this option list and exits";
  "-pp", Arg.Unit (fun () -> Config.set_reprint true),
  " prints the SMT-LIB AST read on stdout";
  "-preLA", Arg.Unit (fun () -> Config.set_preLA true),
  " read SMT-LIB AST, rewrites LA terms and prints on stdout";
  "-debug", Arg.Unit (fun () -> Config.set_debug true),
  " enables debug messages";
  "-multi", Arg.Unit (fun () -> Config.set_pushpop true),
  " generates independent SMTLIB scripts for each (check-sat) command";
  "-disable-success", Arg.Unit (fun () -> Config.set_smtsuccess false),
  " do not print success while parsing";
  "-obfuscate", Arg.Unit (fun () -> Config.set_obfuscate true),
  " generates obfuscated version of SMT script";
  "-keep", Arg.String Config.set_keep_symbols,
  " do not obfuscate this comma separated list of symbols";
  "-version", Arg.Unit Config.pp_version,
  " prints version number";
  "-detect-logic", Arg.Unit (fun () -> Config.set_detect true),
  " infer the logic used by SMT-LIB script (alpha)";
  "-undef-unused", Arg.Unit (fun () -> Config.set_unused true),
  " infer the logic used by SMT-LIB script (alpha)";
]

and print_usage () =
  Arg.usage (Arg.align argspec) umsg;
  exit 0;
;;

let main () =
  Config.set_preprocessor true;
  Arg.parse argspec Config.set_file umsg;
  Do_parse.apply ()
;;

main ()
