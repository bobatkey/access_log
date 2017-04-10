(** Representation, parsing, and printing of Common Log Format. *)

(**{1 Common Log Format}

   This module provides types for representing HTTP server access log
   entries in Common Log Format with extensions. *)

(**{2 Representation of Log Entries} *)

type request_line =
  { meth         : Cohttp.Code.meth
  ; resource     : string
  ; http_version : Cohttp.Code.version
  }

type access_log_entry =
  { addr         : Ipaddr.V4.t
  ; userid       : string option
  ; timestamp    : Ptime.t
  ; request_line : [ `Parsed of request_line | `Unparsed of string ]
  ; status       : Cohttp.Code.status_code
  ; length       : int
  ; referrer     : string option
  ; user_agent   : string option
  }

(**{2 Request lines} *)

module RequestLine : sig

  type t = request_line =
    { meth         : Cohttp.Code.meth
    ; resource     : string
    ; http_version : Cohttp.Code.version
    } [@@deriving fields, ord, eq]

  val make :
    meth:Cohttp.Code.meth ->
    resource:string ->
    ?http_version:Cohttp.Code.version ->
    unit ->
    request_line

  val to_string : request_line -> string

  val of_string : string -> request_line option

end

(**{2 Log entries} *)

module Entry : sig

  type t = access_log_entry =
    { addr         : Ipaddr.V4.t
    ; userid       : string option
    ; timestamp    : Ptime.t
    ; request_line : [ `Parsed of request_line | `Unparsed of string ]
    ; status       : Cohttp.Code.status_code
    ; length       : int
    ; referrer     : string option
    ; user_agent   : string option
    } [@@deriving fields, ord, eq]

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
    access_log_entry

  val read_entry :
    Lexing.lexbuf ->
    [ `Line of access_log_entry
    | `Parse_error_on_line of int
    | `End_of_input ]

  val read_until_eof : Lexing.lexbuf -> access_log_entry list * int list

  val of_string : string -> [ `Line of access_log_entry | `Parse_error ]

  val of_file : string -> access_log_entry list * int list

  val output : ?tz_offset_s:int -> out_channel -> access_log_entry -> unit

  val to_string : ?tz_offset_s:int -> access_log_entry -> string

  val pp : Format.formatter -> access_log_entry -> unit

end
