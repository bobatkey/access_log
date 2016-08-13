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
      (* FIXME: return the string if this doesn't work *)
      Ptime.of_date_time
        ((year, month, day),
	 ((hours, minutes, seconds), tz_offset)) }

and optional_natural = parse
| digit+ as digits { Some (int_of_string digits) }
| '-'              { None }

and natural = parse
| digit+ as digits { int_of_string digits }

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
| "\\x" (hex hex as code)
                       { Buffer.add_char buf (Char.chr (Scanf.sscanf code "%x" (fun x -> x)));
                         quoted_string_text buf lexbuf }

and ws = parse
| [' ''\t']+ { () }

and newline = parse
| '\n' { () }

and dash = parse
| '-' { () }

and until_newline = parse
| [^'\n']* '\n' { () }
| [^'\n']* eof  { () }

{
let status_code lexbuf =
  Cohttp.Code.status_of_code (natural lexbuf)

type logline =
  { addr         : Ipaddr.V4.t
  ; timestamp    : Ptime.t option
  ; request_line : string
  ; status       : Cohttp.Code.status_code
  ; length       : int
  ; referrer     : string
  ; user_agent   : string
  }

let logline lexbuf =
  try
    let addr      = ipv4_address lexbuf in
    let ()        = ws lexbuf in
    let ()        = dash lexbuf in
    let ()        = ws lexbuf in
    let ()        = dash lexbuf in
    let ()        = ws lexbuf in
    let timestamp = datetime lexbuf in
    let ()        = ws lexbuf in
    let req       = quoted_string lexbuf in
    let ()        = ws lexbuf in
    let status    = status_code lexbuf in
    let ()        = ws lexbuf in
    let length    = natural lexbuf in
    let ()        = ws lexbuf in
    let referrer  = quoted_string lexbuf in
    let ()        = ws lexbuf in
    let ua        = quoted_string lexbuf in
    let ()        = newline lexbuf in
    `Line { addr; timestamp; request_line = req; status
          ; length; referrer; user_agent = ua }
  with
   | Failure _ ->
      let () = until_newline lexbuf in
      `Parse_error
   | End_of_file ->
      `End_of_input
}
