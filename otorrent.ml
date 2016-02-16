open Core.Std;;
open Async.Std;;

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

let extract_length info : int option =
  Option.bind (Bencode.dict_get info "length") Bencode.as_int
in

let build_tracker_query response : (Uri.t, string) Result.t =
  match extract_info response with
  | None -> Error "Failed to extract info\n\n"
  | Some info ->
      let info_str = Bencode.encode_to_string info in
      let info_hash = sha1_encode info_str in
      print_string ("INFO_HASH: " ^ (Cryptokit.transform_string (Cryptokit.Hexa.encode ()) info_hash) ^ "\n\n");
    match extract_tracker_url response with
    | None -> Error "Failed to extract tracker url\n\n"
    | Some tracker_url ->
        match extract_length info with
      | None -> Error "Failed to extract length\n\n"
      | Some length ->
          print_string ("tracker url: " ^ tracker_url ^ "\n\n");
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
              print_string ((Uri.to_string uri) ^ "\n\n");
    Cohttp_async.Client.get uri
    >>= fun (_, body) ->
      Cohttp_async.Body.to_string body
    >>| fun (body_string) ->
      print_string (body_string ^ "\n\n")
        in
  never_returns (Scheduler.go ())

