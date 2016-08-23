open Lwt

let () = Lwt_log.add_rule "*" Lwt_log.Info

type redis_value =
  | Integer of int64
  | Error of string
  | String of string
  | BulkStrings of bytes
  | Array of redis_value list

let map f x =
  match x with
  | Some x -> Some (f x)
  | None -> None

let read_simple_string ic =
  Lwt_io.read_line_opt ic >>=
  (fun line ->
     match line with
     | Some x -> return @@ Some (String x)
     | None -> failwith "error")

let read_error ic =
  Lwt_io.read_line_opt ic >>=
  (fun line ->
     return @@ map (fun x -> Error x) line)

let read_integer ic =
  Lwt_io.read_line_opt ic >>=
  (fun line ->
    return @@ map (fun x -> Int64.of_string x |> (fun y -> Integer y)) line)

let read_bulk_string ic =
  read_integer ic >>=
  function
  | Some x ->
    let x = match x with
      | Integer x -> x
      | _ -> failwith "error"  in
    let data = Lwt_io.read ~count:(Int64.to_int x) ic >>= (fun data -> Some (BulkStrings data) |> return) in
    Lwt_io.read ~count:2 ic >>= fun _ -> data
  | None -> fail_with "error"

let deoptionalize l =
  let rec deopt acc = function
    | [] -> List.rev acc
    | None::tl -> deopt acc tl
    | Some x::tl -> deopt (x::acc) tl
  in
  deopt [] l

let rec read_redis ic =
  Lwt_io.read_char_opt ic >>=
  (function
     | Some '+' -> read_simple_string ic
     | Some '-' -> read_error ic
     | Some ':' -> read_integer ic
     | Some '$' -> read_bulk_string ic
     | Some '*' -> read_array ic
     | _ -> return_none)

and read_array ic =
  read_integer ic >>=
  function
  | Some (Integer -1L) -> return_none
  | Some (Integer x) ->
    let rec f n acc =
      if n < 1 then return acc else
        (read_redis ic >>= fun x -> f (n-1) (x :: acc)) in
    let items = f (Int64.to_int x) [] in

    items >>= fun list ->
      return @@ Some (Array(deoptionalize list))
  | _ -> return_none


let handle_message msg =
  match msg with
  | Integer x -> Printf.sprintf "integer %l" (Int64.to_int x)
  | Error _ -> "error"
  | String s -> s
  | BulkStrings _ -> "bulk strings"
  | Array _ -> "array"

let rec handle_connection ic oc () =
  read_redis ic >>=
  (fun msg ->
     match msg with
     | Some msg ->
       let reply = handle_message msg in
       Lwt_io.write_line oc reply >>= handle_connection ic oc
     | None -> Lwt_log.info "Connection closed" >>= return)

let accept_connection conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd Lwt_io.Input fd in
  let oc = Lwt_io.of_fd Lwt_io.Output fd in
  Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
  Lwt_log.info "New Connection" >>= return

let create_server sock =
  let rec serve () =
    Lwt_unix.accept sock >>= accept_connection >>= serve
    in serve

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock @@ ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", 9000);
  listen sock 10;
  sock

let () =
  let sock = create_socket () in
  let serve = create_server sock in
  Lwt_main.run @@ serve ()
