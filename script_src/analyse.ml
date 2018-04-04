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
  Re.compile
    Re.(seq [ rep any; alt (List.map str strs); rep any ])

let filtered_agents =
  contains_string_re [ "bot"; "spider"; "Flip" ]

let extensions =
  contains_string_re [ "pdf"; "html"; "css"; "bib" ]

let opt_default d = function None -> d | Some s -> s

let analyse_entry = function
  | { Access_log.request_line =
        `Parsed { Access_log.meth = `GET; resource }
    ; status
    ; addr
    ; user_agent
    ; timestamp }
    when not (Re.execp filtered_agents (opt_default "" user_agent))
      && status <> `Partial_content
      && (Re.execp extensions resource) ->
     Some ( timestamp
          , addr
          , gethostbyaddr addr
          , resource )
  | _ ->
     None

type 'a gen = ('a -> unit) -> unit

let map_filter f g : 'a gen =
  fun k ->
    g (fun x -> match f x with None -> () | Some x -> k x)

let tz_offset_s = Ptime_clock.current_tz_offset_s ()

let renderer (time, addr, host, path) =
  Printf.printf "%s %s %s\n%!"
    (Ptime.to_rfc3339 ~space:true ?tz_offset_s time)
    (opt_default (Ipaddr.V4.to_string addr) host)
    path

let of_logfile filename : Access_log.entry gen =
  fun k ->
    let ch = open_in filename in
    let lb = Lexing.from_channel ch in
    let rec loop () =
      match Access_log.Entry.read_entry lb with
        | `Parse_error_on_line _ -> loop ()
        | `End_of_input -> ()
        | `Line l -> k l; loop ()
    in
    try loop (); close_in ch with e -> close_in ch; raise e

let loglines_of_stdin k =
  let lb = Lexing.from_channel stdin in
  let rec loop () =
    match Access_log.Entry.read_entry lb with
      | `Parse_error_on_line _ -> loop ()
      | `End_of_input -> ()
      | `Line l -> k l; loop ()
  in
  loop ()

let sink_to f g = g f

let _ =
  loglines_of_stdin
  |> map_filter analyse_entry
  |> sink_to renderer
