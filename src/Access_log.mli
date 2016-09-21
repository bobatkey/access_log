
(**{2 Request lines} *)

type request_line =
  { meth         : Cohttp.Code.meth
  ; resource     : string
  ; http_version : Cohttp.Code.version
  }

module RequestLine : sig
  type t = request_line

  val make :
    meth:Cohttp.Code.meth ->
    resource:string ->
    ?http_version:Cohttp.Code.version ->
    unit ->
    request_line

  val to_string : request_line -> string

  val of_string : string -> request_line option

  val meth : request_line -> Cohttp.Code.meth

  val resource : request_line -> string

  val http_version : request_line -> Cohttp.Code.version
end

(**{2 Log entries} *)

type entry =
  { addr         : Ipaddr.V4.t
  ; userid       : string option
  ; timestamp    : Ptime.t
  ; request_line : [ `Parsed of request_line | `Unparsed of string ]
  ; status       : Cohttp.Code.status_code
  ; length       : int
  ; referrer     : string option
  ; user_agent   : string option
  }

module Entry : sig

  val make :
    addr:Ipaddr.V4.t ->
    ?userid:string ->
    timestamp:Ptime.t ->
    request_line:[ `Parsed of request_line | `Unparsed of string ] ->
    status:Cohttp.Code.status_code ->
    length:int ->
    ?referrer:string ->
    ?user_agent:string ->
    unit ->
    entry

  val addr : entry -> Ipaddr.V4.t

  val userid : entry -> string option

  val timestamp : entry -> Ptime.t

  val request_line : entry -> [ `Parsed of request_line | `Unparsed of string ]

  val status : entry -> Cohttp.Code.status_code

  val length : entry -> int

  val referrer : entry -> string option

  val user_agent : entry -> string option

  val read_entry :
    Lexing.lexbuf ->
    [ `Line of entry
    | `Parse_error_on_line of int
    | `End_of_input ]

  val read_until_eof : Lexing.lexbuf -> entry list * int list

  val of_string : string -> [ `Line of entry | `Parse_error ]

  val of_file : string -> entry list * int list

  val output : ?tz_offset_s:int -> out_channel -> entry -> unit

  val to_string : ?tz_offset_s:int -> entry -> string

  val pp : Format.formatter -> entry -> unit
end
