open Lwt

type resp_value =
  | Integer of int64
  | Error of string
  | String of string
  | BulkString of bytes
  | Array of resp_value option list

let map_opt f x = function
  | Some(xx) -> return_some (f xx)
  | None -> return_none

let read_simple_string ic =
  Lwt_io.read_line_opt ic >>= function
  | Some line -> return_some (String line)
  | None -> return_none

let read_error ic =
  Lwt_io.read_line_opt ic >>= function
  | Some line -> return_some (Error line)
  | None -> return_none

let read_integer ic =
  Lwt_io.read_line_opt ic >>= function
  | Some line -> return_some (Int64.of_string line |> (fun y -> (Integer y)))
  | None -> return_none

let read_bulk_string ic =
  read_integer ic >>= function
  | Some x ->
    let x = match x with
      | Integer x -> x
      | _ -> 0L in
    if x == 0L then
      return_none
    else
      let data = Lwt_io.read ~count:(Int64.to_int x) ic >>= (fun data -> Some (BulkString data) |> return) in
      Lwt_io.read ~count:2 ic >>= fun _ -> data
  | None -> fail_with "error"

let rec read_redis ic =
  Lwt_io.read_char_opt ic >>= function
  | Some '+' -> read_simple_string ic
  | Some '-' -> read_error ic
  | Some ':' -> read_integer ic
  | Some '$' -> read_bulk_string ic
  | Some '*' -> read_array ic
  | Some x -> fail_invalid_arg @@ Printf.sprintf "Uknown argument %c" x
  | None -> return_none

and read_array ic =
  read_integer ic >>= function
  | Some (Integer -1L) -> return_none
  | Some (Integer x) ->
    let rec f n acc =
      if n < 1 then return @@ List.rev acc else
        read_redis ic >>= fun x -> f (n-1) (x :: acc) in
    f (Int64.to_int x) [] >>= fun list ->
    return @@ Some (Array list)
  | _ -> return_none

let write_with_newline oc x =
  let crlf = String.make 1 '\r' ^ String.make 1 '\n' in
  crlf::x::oc

let prefix_char s c = String.make 1 c ^ s

let write_integer oc x =
  write_with_newline oc (prefix_char (Int64.to_string x) ':')

let write_string oc x =
  write_with_newline oc (prefix_char x '+')

let write_error oc x =
  write_with_newline oc (prefix_char x '-')

let write_bulkstring oc x =
  write_with_newline (write_with_newline oc (Printf.sprintf "$%d" (Bytes.length x))) x

let write_none oc =
  write_with_newline oc "$-1"

let rec write_redis oc value =
  match value with
  | Integer x -> write_integer oc x
  | String x -> write_string oc x
  | Error x -> write_error oc x
  | BulkString x -> write_bulkstring oc x
  | Array x -> write_array oc x

and write_array oc x =
  let oc' = write_with_newline oc (Printf.sprintf "*%d" (List.length x)) in
  List.fold_left (fun acc i ->
      match i with
      | None -> write_none acc
      | Some(v) -> write_redis acc v) oc' x

let write_value oc value =
  write_redis [] value
  |> List.rev
  |> Lwt_list.iter_s (Lwt_io.write oc)

let rec pprint x =
  match x with
  | Integer x -> Printf.sprintf "int[%s]" (Int64.to_string x)
  | String x -> Printf.sprintf "string[%s]" x
  | Error x -> Printf.sprintf "error[%s]" x
  | BulkString x -> Printf.sprintf "bulk_string[%s]" (Bytes.to_string x)
  | Array x ->
    List.fold_left (fun acc item ->
        match item with
        | None -> acc ^ "None, "
        | Some(v) -> acc ^ (pprint v) ^ ", ")
      "array[" x
