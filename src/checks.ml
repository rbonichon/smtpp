module type Check = sig
    val check_constant : Ast.constant -> Ast.constant ;;
    val check_symbol : Ast.symbol -> Ast.symbol ;;
    val check_term : Ast.term -> Ast.term ;;
    val check_command : Ast.command -> Ast.command ;;
end


module Combine(C1: Check)(C2 : Check) : Check = struct
    let check_constant c = C2.check_constant (C1.check_constant c) ;;
    let check_symbol s = C2.check_symbol (C1.check_symbol s) ;;
    let check_term t = C2.check_term (C1.check_term t) ;;
    let check_command cmd = C2.check_command (C1.check_command cmd);;
end
