module E = Access_log.Entry
module R = Access_log.RequestLine


let string x = `String x
let int x = `Int x
let option f = function
  | None -> `Null
  | Some x -> f x


let json_of_request_line rl =
  `Assoc [ "method", string (Access_log.string_of_method (R.meth rl))
         ; "resource", string (R.resource rl)
         ; "http_version", string (Access_log.string_of_version (R.http_version rl))
         ]

let json_of_entry entry =
  `Assoc [ "addr",       string (Ipaddr.V4.to_string (E.addr entry))
         ; "timestamp" , string (Ptime.to_rfc3339 (E.timestamp entry))
         ; "request_line",
           (match E.request_line entry with
            | `Parsed reql  -> json_of_request_line reql
            | `Unparsed str -> string str)
         ; "status",     int (E.status entry)
         ; "length",     int (E.length entry)
         ; "referrer",   option string (E.referrer entry)
         ; "user_agent", option string (E.user_agent entry)
         ]

let x =
  stdin
  |> Access_log.seq_of_channel
  |> Seq.map
    (function
      | Ok entry -> Some (json_of_entry entry)
      | Error (`Line i) ->
        Printf.eprintf "Error on line %d\n%!" i; None)
  |> Seq.filter_map (fun x -> x)
  |> List.of_seq
  |> (fun items -> `List items)
  |> Yojson.Basic.pretty_to_channel stdout
