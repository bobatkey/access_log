type logline =
  { addr         : Ipaddr.V4.t
  ; timestamp    : Ptime.t option
  ; request_line : string
  ; status       : Cohttp.Code.status_code
  ; length       : int
  ; referrer     : string
  ; user_agent   : string
  }

val logline :
  Lexing.lexbuf ->
  [ `Line of logline
  | `Parse_error
  | `End_of_input ]
