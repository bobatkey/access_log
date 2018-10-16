let host_cache = Hashtbl.create 128

let gethostbyaddr addr =
  match Hashtbl.find host_cache addr with
    | exception Not_found ->
       let cmd = Printf.sprintf "dig +short -x %s" (Ipaddr.V4.to_string addr) in
       let ch = Unix.open_process_in cmd in
       let hostname =
         match input_line ch with
           | exception End_of_file -> None
           | line                  -> Some line
       in
       ignore (Unix.close_process_in ch);
       Hashtbl.add host_cache addr hostname;
       hostname
    | hostname ->
       hostname

let contains_string_re strs =
  Re.(compile (seq [ rep any; alt (List.map str strs); rep any ]))

let filtered_agents = contains_string_re [ "bot"; "spider"; "Flip" ]

let extensions = contains_string_re [ "pdf"; "html"; "css"; "bib" ]

let opt_default d = function None -> d | Some s -> s

let analyse_entry = function
  | Access_log.{ request_line = `Parsed { meth = `GET; resource; _ }
               ; status; addr; user_agent; timestamp; _ }
    when not (Re.execp filtered_agents (opt_default "" user_agent))
      && status <> `Partial_content
      && (Re.execp extensions resource) ->
     Some ( timestamp
          , addr
          , gethostbyaddr addr
          , resource )
  | _ ->
     None

let tz_offset_s = Ptime_clock.current_tz_offset_s ()

let renderer (time, addr, host, path) =
  Printf.printf "%s %s %s\n%!"
    (Ptime.to_rfc3339 ~space:true ?tz_offset_s time)
    (opt_default (Ipaddr.V4.to_string addr) host)
    path

let _ =
  Access_log.seq_of_channel stdin
  |> Seq.filter_map analyse_entry
  |> Seq.iter renderer
