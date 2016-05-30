val set_debug : bool -> unit
val get_debug : unit -> bool

val set_pushpop : bool -> unit
val get_pushpop : unit -> bool

val set_smtsuccess : bool -> unit
val get_smtsuccess : unit -> bool

val set_reprint : bool -> unit
val get_reprint : unit -> bool

val set_preLA : bool -> unit
val get_preLA : unit -> bool

val set_preprocessor : bool -> unit
val get_preprocessor : unit -> bool

val set_unused : bool -> unit
val get_unused : unit -> bool

val set_detect : bool -> unit
val get_detect : unit -> bool

val set_obfuscate : bool -> unit
val get_obfuscate : unit -> bool

val set_keep_symbols : string -> unit
val get_keep_symbols : unit -> string list

val get_files : unit -> string list
val set_file : string -> unit
  
val pp_version : unit -> unit 
