{
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
  | _     -> assert false

let int_of_char c = Char.code c - Char.code '0'
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

let hex = digit | ['A'-'Z''a'-'z']

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

and natural = parse
| digit+ as digits { int_of_string digits }

and natural_or_dash = parse
| digit+ as digits { int_of_string digits }
| '-'              { 0 }

and quoted_string = parse
| '"' { let b = Buffer.create 256 in
        quoted_string_text b lexbuf }

(* FIXME: what about actual newlines, tabs, etc.? 
   and what other characters may be escapes? *)
and quoted_string_text buf = parse
| [^'\\''"']* as chunk { Buffer.add_string buf chunk; quoted_string_text buf lexbuf }
| '"'                  { Buffer.contents buf }
| "\\n"                { Buffer.add_char buf '\n'; quoted_string_text buf lexbuf }
| "\\t"                { Buffer.add_char buf '\t'; quoted_string_text buf lexbuf }
| "\\\\"               { Buffer.add_char buf '\\'; quoted_string_text buf lexbuf }
| "\\x" (hex hex as code)
                       { Buffer.add_char buf (Char.chr (Scanf.sscanf code "%x" (fun x -> x)));
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
  ' ' ([^' ']+ as target)
  ' ' "HTTP/" (digit as major) '.' (digit as minor)
  { (Cohttp.Code.method_of_string method_str, target,
     (int_of_char major, int_of_char minor)) }

{
let status_code lexbuf =
  Cohttp.Code.status_of_code (natural lexbuf)

type logline =
  { addr         : Ipaddr.V4.t
  ; userid       : string option
  ; timestamp    : Ptime.t
  ; request_line : [ `Parsed of Cohttp.Code.meth * string * (int * int) | `Unparsed of string ]
  ; status       : Cohttp.Code.status_code
  ; length       : int
  ; referrer     : string option
  ; user_agent   : string option
  }

let parse_request_line str =
  let lb = Lexing.from_string str in
  try `Parsed (request_line lb) with Failure _ -> `Unparsed str

let dash_means_None = function
  | "-" -> None
  | str -> Some str

let logline lexbuf =
  try
    let addr      = ipv4_address lexbuf in
    let ()        = ws lexbuf in
    let ()        = dash lexbuf in
    let ()        = ws lexbuf in
    let userid    = dash_means_None (non_ws lexbuf) in
    let ()        = ws lexbuf in
    let timestamp = datetime lexbuf in
    let ()        = ws lexbuf in
    let req       = parse_request_line (quoted_string lexbuf) in
    let ()        = ws lexbuf in
    let status    = status_code lexbuf in
    let ()        = ws lexbuf in
    let length    = natural_or_dash lexbuf in
    let ()        = ws lexbuf in
    let referrer  = quoted_string lexbuf |> dash_means_None in
    let ()        = ws lexbuf in
    let ua        = quoted_string lexbuf |> dash_means_None in
    let ()        = newline lexbuf in
    `Line { addr; userid; timestamp; request_line = req; status
          ; length; referrer; user_agent = ua }
  with
    | Failure _ ->
       let {Lexing.pos_lnum} = Lexing.lexeme_start_p lexbuf in
       let () = until_newline lexbuf in
       `Parse_error_on_line (pos_lnum+1)
    | End_of_file ->
       `End_of_input

let addr {addr} = addr
let userid {userid} = userid
let timestamp {timestamp} = timestamp
let request_line {request_line} = request_line
let status {status} = status
let length {length} = length
let referrer {referrer} = referrer
let user_agent {user_agent} = user_agent

let loglines lb =
  let rec loop errors accum =
    match logline lb with
      | `End_of_input             -> List.rev accum, List.rev errors
      | `Parse_error_on_line lnum -> loop (lnum::errors) accum
      | `Line line                -> loop errors (line::accum)
  in
  loop [] []

let loglines_of_file filename =
  let ch = open_in filename in
  try
    let lb = Lexing.from_channel ch in
    let l  = loglines lb in
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

let output_timestamp ?tz_offset_s ch ptime =
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
  Printf.fprintf ch "[%02d/%s/%04d:%02d:%02d:%02d %c%02d%02d]"
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

let output ?tz_offset_s ch logline =
  Printf.fprintf ch "%s - %s "
    (Ipaddr.V4.to_string logline.addr)
    (none_means_dash logline.userid);
  output_timestamp ?tz_offset_s ch logline.timestamp;
  (match logline.request_line with
    | `Parsed (meth, path, (major,minor)) ->
       Printf.fprintf ch " \"%s %s HTTP/%d.%d\" "
         (Cohttp.Code.string_of_method meth)
         (escape_string path)
         major
         minor
    | `Unparsed str ->
       Printf.fprintf ch " \"%s\" " (escape_string str));
  Printf.fprintf ch "%d %d \"%s\" \"%s\"\n"
    (Cohttp.Code.code_of_status logline.status)
    logline.length
    (match logline.referrer with None -> "-" | Some s -> escape_string s)
    (match logline.user_agent with None -> "-" | Some s -> escape_string s)
}
