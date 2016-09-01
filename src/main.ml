open Lwt

let () = Lwt_log.add_rule "*" Lwt_log.Info

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None::tl -> deopt acc tl
    | Some x::tl -> deopt (x::acc) tl
  in
  deopt [] l

let to_string msg: string =
  let m = match msg with
  | Resp.Integer x -> Printf.sprintf "integer: %s" (Int64.to_string x)
  | Resp.Error s -> Printf.sprintf "error: %s" s
  | Resp.String s -> Printf.sprintf "string: %s" s
  | Resp.BulkString b -> Printf.sprintf "bulk string: %d bytes" (Bytes.length b)
  | Resp.Array _ -> "array" in
  m

let rec handle_connection db ic oc () =
  Resp.read_redis ic >>= function
  | Some msg ->
    Lwt_log.info_f "incoming message: %s" (Resp.pprint msg) >>= fun _ ->
    let msg = Redis.parse_request msg in
    Resp.write_value oc msg >>= handle_connection db ic oc
  | None -> Lwt_log.info "Connection closed" >>= return

let accept_connection db conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_connection db ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "New Connection" >>= return

let create_server db sock =
  let rec serve () =
    Lwt_unix.accept sock >>= (accept_connection db) >>= serve
    in serve

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", 9000);
  listen sock 10;
  sock

let () =
  let db = Redis.create in
  let sock = create_socket () in
  let serve = create_server db sock in
  Lwt_main.run @@ serve ()
