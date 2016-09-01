open Core.Std

type cache_node = {
    insert_time: float;
    value: string;
  }

module CacheNode = struct
  type t = {
      insert_time: Time.t;
      time_to_live: Time.t;
      key: string;
      value: string;
    }
end

module Command = struct
  let details = Resp.Array [ Some(Resp.BulkString "SET");
                             Some(Resp.Integer (-3L));
                             Some(Resp.Array [ Some(Resp.BulkString "write");Some(Resp.BulkString "denyoom")]) ;
                             Some(Resp.Integer 1L);
                             Some(Resp.Integer 1L);
                             Some(Resp.Integer 1L);
                           ]
  type t = {
    key: string;
    value: string;
  }
end

type commands =
  | Set of string * string
  | Get of string
  | Command of string

let create =
  String.Map.empty

let list_all_commands =
  Resp.Array[ Some(Command.details) ]

let parse_request ro =
  let ar = match ro with
    | Resp.Array x -> x
    | _ -> failwith "invalid" in

  let cmd_string = match ar with
    | Some(Resp.BulkString x)::tl -> (BytesLabels.to_string x)
    | _ -> failwith "invalid" in

  match cmd_string with
  | "COMMAND" -> list_all_commands
  (* | "SET" -> handle_set_command ar *)
  | _ -> failwith "notimplmeented"
