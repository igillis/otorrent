open Core.Std;;
open Async.Std;;

let to_hex = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) in
let sha1_encode str =
  let open Cryptokit in
  hash_string (Hash.sha1 ()) str
  in

  let extract_info response : Bencode.t option =
    Bencode.dict_get response "info"
in

let extract_tracker_url response : string option =
  Option.bind (Bencode.dict_get response "announce") Bencode.as_string
  in

  (* TODO: can probably extract out this option.bind clause *)
  let extract_length info : int option =
    Option.bind (Bencode.dict_get info "length") Bencode.as_int
in

let extract_ip_port peer : ((string * int), string) Result.t =
  match Option.bind (Bencode.dict_get peer "ip") Bencode.as_string with
  | None -> Error "Could not extract ip from peer"
  | Some ip ->
      match Option.bind (Bencode.dict_get peer "port") Bencode.as_int with
    | None -> Error "Could not extract port from peer"
    | Some port -> Ok (ip, port)
  in

  let extract_ip_ports body_string : ((string * int) list, string) Result.t =
    let bencoded_body = Bencode.decode (`String body_string) in
    match Option.bind (Bencode.dict_get bencoded_body "peers") Bencode.as_list with
  | None -> Error "Failed to extract peers\n\n"
  | Some peer_list ->
      List.fold_left peer_list ~init:(Ok []) ~f:(fun acc res ->
        match acc, res with
        | (Error s), _ -> Error s
        | (Ok ip_ports), (peer) ->
            match (extract_ip_port peer) with
            | Error s -> Error s
            | Ok ip_port -> Ok (ip_port::ip_ports)
            )
    in

let build_tracker_query response : (Uri.t, string) Result.t =
  match extract_info response with
  | None -> Error "Failed to extract info\n\n"
  | Some info ->
    let info_str = Bencode.encode_to_string info in
    let info_hash = sha1_encode info_str in
    print_string ("[INFO_HASH]\n " ^ (to_hex info_hash) ^ "\n\n");
    match extract_tracker_url response with
    | None -> Error "Failed to extract tracker url\n\n"
    | Some tracker_url ->
        match extract_length info with
      | None -> Error "Failed to extract length\n\n"
      | Some length ->
          let tracker_uri = Uri.of_string tracker_url in
          let query_params = [
            ("info_hash", info_hash);
            ("event", "started");
            ("peer_id", "asdfgQweRRyuioplk87x");
            ("uploaded", "0");
            ("downloaded", "0");
            ("left", Int.to_string length);
            ("port", "6881")
          ] in
            Ok (Uri.add_query_params' tracker_uri query_params)
in
let make_tracker_request tracker_url =
  let res = Bencode.decode (`File_path tracker_url) in
    build_tracker_query res
in

let _ = match (make_tracker_request "./579.mp3.torrent") with
| Error e -> return (print_string ("Error making tracker query: " ^ e ^ "\n\n"))
| Ok uri ->
    Cohttp_async.Client.get uri
    >>= fun (_, body) ->
    Cohttp_async.Body.to_string body
    >>| fun (body_string) ->
    match extract_ip_ports body_string with
    | Error e -> print_string ("Error extracting ip ports: " ^ e ^ "\n\n")
    | Ok ip_port_list ->
      let ip_port_strings = List.map ip_port_list ~f:(fun (ip, port) ->
        (Ipaddr.V4.to_string (Ipaddr.V4.of_bytes_exn ip)) ^ ":" ^ (Int.to_string port)) in
      print_string ("[IP:PORT]\n" ^ (String.concat ~sep:"\n" ip_port_strings) ^ "\n\n")
in
  never_returns (Scheduler.go ())

