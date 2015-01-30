type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
}
;;

let in_file name =
  let loc = {
    Lexing.pos_fname = name;
    Lexing.pos_lnum = 1;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = -1;
  }
  in
  { loc_start = loc; loc_end = loc; }
;;

let none = in_file "_none_";;

let dummy_loc = { loc_start = Lexing.dummy_pos; loc_end = Lexing.dummy_pos; }
;;
