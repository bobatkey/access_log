{
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

module Lexer = struct

let month_of_string = function
  | "Jan" -> 1
  | "Feb" -> 2
  | "Mar" -> 3
  | "Apr" -> 4
  | "May" -> 5
  | "Jun" -> 6
  | "Jul" -> 7
  | "Aug" -> 8
  | "Sep" -> 9
  | "Oct" -> 10
  | "Nov" -> 11
  | "Dec" -> 12
  | _     -> invalid_arg "month_of_string"

let int_of_hexdigit c =
  match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | _        -> invalid_arg "int_of_hexdigit"
}

let digit = ['0'-'9']

let ipv4_component =
    ('2' '5' ['0'-'5'])
  | ('2' ['0'-'4'] digit)
  | ('1' digit digit)
  | ('0' digit digit)
  | (digit digit)
  | digit

let ipv4_address =
  ipv4_component '.' ipv4_component '.' ipv4_component '.' ipv4_component

let month =
  "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun"
| "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"

let hex = digit | ['A'-'F''a'-'f']

(* Parsing rules *)

rule ipv4_address = parse
| ipv4_address as str
    { Ipaddr.V4.of_string_exn str }
| eof
    { raise End_of_file }

and datetime = parse
| '[' (digit digit as day)
  '/' (month as month)
  '/' (digit digit digit digit as year)
  ':' (digit digit as hours)
  ':' (digit digit as minutes)
  ':' (digit digit as seconds)
  ' ' (('-' | '+') as sign)
      (digit digit as tz_hours)
      (digit digit as tz_minutes)
  ']'
    { let day       = int_of_string day
      and month     = month_of_string month
      and year      = int_of_string year
      and hours     = int_of_string hours
      and minutes   = int_of_string minutes
      and seconds   = int_of_string seconds
      and tz_offset =
        (if sign = '-' then -60 else 60)
        * (int_of_string tz_hours * 60 + int_of_string tz_minutes)
      in
      let date = year, month, day in
      let time = (hours, minutes, seconds), tz_offset in
      match Ptime.of_date_time (date, time) with
        | None       -> failwith ""
        | Some ptime -> ptime }

and posint = parse
| digit+ as digits { int_of_string digits (* FIXME: overflow? *)}

and posint_or_dash = parse
| digit+ as digits { int_of_string digits (* FIXME: overflow? *)}
| '-'              { 0 }

and quoted_string = parse
| '"' { let b = Buffer.create 256 in
        quoted_string_text b lexbuf }

(* FIXME: what about actual newlines, tabs, etc.? 
   and what other characters may be escapes? *)
and quoted_string_text buf = parse
| [^'\\''"']* as chunk
         { Buffer.add_string buf chunk; quoted_string_text buf lexbuf }
| '"'    { Buffer.contents buf }
| "\\n"  { Buffer.add_char buf '\n'; quoted_string_text buf lexbuf }
| "\\t"  { Buffer.add_char buf '\t'; quoted_string_text buf lexbuf }
| "\\\\" { Buffer.add_char buf '\\'; quoted_string_text buf lexbuf }
| "\\\"" { Buffer.add_char buf '\"'; quoted_string_text buf lexbuf }
| "\\x" (hex as hi) (hex as lo)
    { let hi = int_of_hexdigit hi
      and lo = int_of_hexdigit lo in
      let c  = Char.chr (hi * 16 + lo) in
      Buffer.add_char buf c;
      quoted_string_text buf lexbuf }

and ws = parse
| [' ''\t']+ { () }

and newline = parse
| '\n' { Lexing.new_line lexbuf; () }

and dash = parse
| '-' { () }

and non_ws = parse
| [^' ''\t''\n']+ { Lexing.lexeme lexbuf }

and until_newline = parse
| [^'\n']* '\n' { Lexing.new_line lexbuf; () }
| [^'\n']* eof  { () }

and request_line = parse
|     ([^' ']+ as method_str)
  ' ' ([^' ']+ as resource)
  ' ' ([^' ']+ as http_version)
  { { meth = Cohttp.Code.method_of_string method_str
    ; resource
    ; http_version = Cohttp.Code.version_of_string http_version
    } }

{
  let status_code lexbuf =
    Cohttp.Code.status_of_code (posint lexbuf)

  let dash_means_None = function
    | "-" -> None
    | str -> Some str
end

module RequestLine = struct
  type t = request_line

  let make ~meth ~resource ?(http_version=`HTTP_1_1) () =
    { meth; resource; http_version }

  let compare a b =
    match Cohttp.Code.compare_method a.meth b.meth with
      | 0 ->
         (match String.compare a.resource b.resource with
           | 0 -> Cohttp.Code.compare_version a.http_version b.http_version
           | c -> c)
      | c -> c

  let equal a b =
    compare a b = 0
  
  let of_string str =
    let lb = Lexing.from_string str in
    try Some (Lexer.request_line lb) with Failure _ -> None

  let to_string {meth; resource; http_version} =
    Cohttp.Code.string_of_method meth
    ^" "
    ^resource
    ^" "
    ^Cohttp.Code.string_of_version http_version

  let meth {meth} = meth

  let resource {resource} = resource

  let http_version {http_version} = http_version
end

module Entry = struct

  type t = access_log_entry
  
  let make ~addr ?userid ~timestamp ~request_line ~status ~length ?referrer ?user_agent () =
    { addr; userid; timestamp
    ; request_line
    ; status; length; referrer; user_agent
    }

  let addr {addr} = addr

  let userid {userid} = userid

  let timestamp {timestamp} = timestamp

  let request_line {request_line} = request_line

  let status {status} = status

  let length {length} = length

  let referrer {referrer} = referrer

  let user_agent {user_agent} = user_agent

  let read_logline_exn lexbuf =
    let addr      = Lexer.ipv4_address lexbuf in
    let ()        = Lexer.ws lexbuf in
    let ()        = Lexer.dash lexbuf in
    let ()        = Lexer.ws lexbuf in
    let userid    = Lexer.dash_means_None (Lexer.non_ws lexbuf) in
    let ()        = Lexer.ws lexbuf in
    let timestamp = Lexer.datetime lexbuf in
    let ()        = Lexer.ws lexbuf in
    let req       =
      let str = Lexer.quoted_string lexbuf in
      match RequestLine.of_string str with
        | None     -> `Unparsed str
        | Some req -> `Parsed req
    in
    let ()        = Lexer.ws lexbuf in
    let status    = Lexer.status_code lexbuf in
    let ()        = Lexer.ws lexbuf in
    let length    = Lexer.posint_or_dash lexbuf in
    let ()        = Lexer.ws lexbuf in
    let referrer  = Lexer.quoted_string lexbuf |> Lexer.dash_means_None in
    let ()        = Lexer.ws lexbuf in
    let ua        = Lexer.quoted_string lexbuf |> Lexer.dash_means_None in
    { addr; userid; timestamp; request_line = req; status
    ; length; referrer; user_agent = ua }

  let read_entry lexbuf =
    try
      let line = read_logline_exn lexbuf in
      let ()   = Lexer.newline lexbuf in
      `Line line
    with
      | Failure _ ->
         let {Lexing.pos_lnum} = Lexing.lexeme_start_p lexbuf in
         let () = Lexer.until_newline lexbuf in
         `Parse_error_on_line (pos_lnum+1)
      | End_of_file ->
         `End_of_input

  let of_string str =
    let lexbuf = Lexing.from_string str in
    try `Line (read_logline_exn lexbuf)
    with
      | Failure _   -> `Parse_error
      | End_of_file -> `Parse_error

  let read_until_eof lb =
    let rec loop errors accum =
      match read_entry lb with
        | `End_of_input             -> List.rev accum, List.rev errors
        | `Parse_error_on_line lnum -> loop (lnum::errors) accum
        | `Line line                -> loop errors (line::accum)
    in
    loop [] []

  let of_file filename =
    let ch = open_in filename in
    try
      let lb = Lexing.from_channel ch in
      let l  = read_until_eof lb in
      close_in ch; l
    with e -> close_in ch; raise e

  let none_means_dash = function
    | None -> "-"
    | Some str -> str

  let string_of_month = function
    | 1  -> "Jan"
    | 2  -> "Feb"
    | 3  -> "Mar"
    | 4  -> "Apr"
    | 5  -> "May"
    | 6  -> "Jun"
    | 7  -> "Jul"
    | 8  -> "Aug"
    | 9  -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov"
    | 12 -> "Dec"
    | _  -> assert false

  let string_of_timestamp ?tz_offset_s ptime =
    let (year, month, day), ((hours, minutes, seconds), tz_offset_s) =
      Ptime.to_date_time ?tz_offset_s ptime
    in
    let tz_sign, tz_h, tz_m =
      if tz_offset_s mod 60 = 0 then
        let tz_offset_m = abs tz_offset_s / 60 in
        let tz_offset_h = tz_offset_m / 60 in
        let tz_sign = if tz_offset_s < 0 then '-' else '+' in
        tz_sign, tz_offset_h, tz_offset_m mod 60
      else
        '+', 0, 0
    in
    Printf.sprintf "[%02d/%s/%04d:%02d:%02d:%02d %c%02d%02d]"
      day
      (string_of_month month)
      year
      hours
      minutes
      seconds
      tz_sign
      tz_h
      tz_m

  let escape_string s =
    let b = Buffer.create (String.length s * 2) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        | '\"' ->
           Buffer.add_string b "\\\""
        | '\\' ->
           Buffer.add_string b "\\\\"
        | '\n' ->
           Buffer.add_string b "\\n"
        | '\t' ->
           Buffer.add_string b "\\t"
        | '\x00' .. '\x1f' | '\x80' .. '\xff' ->
           Printf.bprintf b "\\x%02X" (Char.code s.[i])
        | c ->
           Buffer.add_char b c
    done;
    Buffer.contents b

  let output_generic ?tz_offset_s emit logline =
    emit (Ipaddr.V4.to_string logline.addr);
    emit " - ";
    emit (none_means_dash logline.userid);
    emit " ";
    emit (string_of_timestamp ?tz_offset_s logline.timestamp);
    emit " \"";
    (match logline.request_line with
      | `Parsed {meth; resource; http_version} ->
         emit (Cohttp.Code.string_of_method meth);
         emit " ";
         emit (escape_string resource);
         emit " ";
         emit (Cohttp.Code.string_of_version http_version)
      | `Unparsed str ->
         emit (escape_string str));
    emit "\" ";
    emit (string_of_int (Cohttp.Code.code_of_status logline.status));
    emit " ";
    emit (string_of_int logline.length);
    emit " \"";
    emit (match logline.referrer with None -> "-"
                                    | Some s -> escape_string s);
    emit "\" \"";
    emit (match logline.user_agent with None -> "-"
                                      | Some s -> escape_string s);
    emit "\""

  let output ?tz_offset_s ch logline =
    output_generic ?tz_offset_s (output_string ch) logline;
    output_char ch '\n'

  let to_string ?tz_offset_s logline =
    let b = Buffer.create 128 in
    output_generic ?tz_offset_s (Buffer.add_string b) logline;
    Buffer.contents b

  let pp_hum ?tz_offset_s () fmt logline =
    output_generic ?tz_offset_s (Format.pp_print_string fmt) logline

  let pp fmt logline =
    pp_hum ~tz_offset_s:0 () fmt logline
end
}
