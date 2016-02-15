open Core.Std;;

let sha1_encode str =
  let open Cryptokit in
  hash_string (Hash.sha1 ()) str
in

let extract_info response =
  match Bencode.dict_get response "info" with
  | None -> None
  | Some info -> Some (Bencode.encode_to_string info)
in

let extract_tracker_url response =
  match Bencode.dict_get response "announce" with
  | None -> None
  | Some tracker_url -> Bencode.as_string tracker_url
in

let build_tracker_query response =
  match extract_info response with
  | None -> Error "Failed to extract info\n"
  | Some info ->
    let info_hash = sha1_encode info in
    print_string ("INFO_HASH: " ^ (Cryptokit.transform_string (Cryptokit.Hexa.encode ()) info_hash) ^ "\n");
    match extract_tracker_url response with
    | None -> Error "Failed to extract tracker url\n"
    | Some tracker_url ->
      print_string ("tracker url: " ^ tracker_url ^ "\n");
      let tracker_uri = Uri.of_string tracker_url in
      Ok (Uri.add_query_param tracker_uri ("info_hash", [info_hash]))
in

let make_tracker_request tracker_url =
  let res = Bencode.decode (`File_path tracker_url) in
  match build_tracker_query res with
  | Error e ->
      print_string ("error building tracker query: " ^ e ^ "\n");
      (Error e)
  | Ok tracker_query ->
    print_string ("tracker query: " ^ (Uri.to_string tracker_query) ^ "\n");
    (Ok tracker_query)
    (* Cohttp_async.Client.get tracker_query *)
    (* >>= fun (_, body) -> *)
    (* Cohttp_async.Body.to_string body *)
    (* >>| fun (body_string) -> *)
    (*   Async.Std.print_string body_string; *)
    (*   Ok body_string *)
in

let _ = match (make_tracker_request "ataleoftwocities00098gut_archive.torrent") with
| Error e -> print_string ("Error making tracker query: " ^ e ^ "\n")
| Ok uri -> print_string ((Uri.to_string uri) ^ "\n")
in ()
  (* >>| fun (body) -> *)
  (*   match body with *)
  (*   | Error e -> Async.Std.print_string e *)
  (*   | Ok s -> Async.Std.print_string s *)
(* in *)
  (* Async.Std.print_string "starting scheduler\n"; *)
  (* Core.Std.never_returns (Scheduler.go ~raise_unhandled_exn:false ()) *)

