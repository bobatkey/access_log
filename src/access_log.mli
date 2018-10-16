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

(**{2 Request lines} *)

module RequestLine : sig

  type t = request_line =
    { meth         : Cohttp.Code.meth
    ; resource     : string
    ; http_version : Cohttp.Code.version [@default `HTTP_1_1]
    } [@@deriving fields, ord, eq, make]

  val to_string : request_line -> string

  val of_string : string -> request_line option

end

(**{2 Log entries} *)

module Entry : sig

  type t = entry =
    { addr         : Ipaddr.V4.t
    ; userid       : string option
    ; timestamp    : Ptime.t
    ; request_line : [ `Parsed of request_line | `Unparsed of string ]
    ; status       : Cohttp.Code.status_code
    ; length       : int
    ; referrer     : string option
    ; user_agent   : string option
    } [@@deriving fields, ord, eq, make]

  val of_string : string -> [ `Line of entry | `Parse_error ]

  val output : ?tz_offset_s:int -> out_channel -> entry -> unit

  val to_string : ?tz_offset_s:int -> entry -> string

  val pp : Format.formatter -> entry -> unit

end

val of_file : string -> entry list * int list

val seq_of_channel : in_channel -> entry Seq.t
