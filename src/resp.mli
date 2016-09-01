type resp_value =
  | Integer of int64
  | Error of string
  | String of string
  | BulkString of bytes
  | Array of resp_value option list

val read_redis: Lwt_io.input_channel -> resp_value option Lwt.t
val write_value: Lwt_io.output_channel -> resp_value -> unit Lwt.t
val pprint: resp_value -> string
