type request_line =
  { meth         : Cohttp.Code.meth
  ; resource     : string
  ; http_version : int * int
  }

type logline =
  { addr         : Ipaddr.V4.t
  ; userid       : string option
  ; timestamp    : Ptime.t
  ; request_line : [ `Parsed of request_line | `Unparsed of string ]
  ; status       : Cohttp.Code.status_code
  ; length       : int
  ; referrer     : string option
  ; user_agent   : string option
  }

val make :
  addr:Ipaddr.V4.t ->
  ?userid:string ->
  timestamp:Ptime.t ->
  meth:Cohttp.Code.meth ->
  resource:string ->
  ?http_version:int * int ->
  status:Cohttp.Code.status_code ->
  length:int ->
  ?referrer:string ->
  ?user_agent:string ->
  unit ->
  logline

val addr : logline -> Ipaddr.V4.t
val userid : logline -> string option
val timestamp : logline -> Ptime.t
val request_line : logline -> [ `Parsed of request_line | `Unparsed of string ]
val status : logline -> Cohttp.Code.status_code
val length : logline -> int
val referrer : logline -> string option
val user_agent : logline -> string option

val logline :
  Lexing.lexbuf ->
  [ `Line of logline
  | `Parse_error_on_line of int
  | `End_of_input ]

val loglines : Lexing.lexbuf -> logline list * int list

val loglines_of_file : string -> logline list * int list

val output : ?tz_offset_s:int -> out_channel -> logline -> unit
