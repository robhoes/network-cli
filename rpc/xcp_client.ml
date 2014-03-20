(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Generic RPC marshalling functions for XCP services *)

module Request = Cohttp.Request.Make(Cohttp_posix_io.Buffered_IO)
module Response = Cohttp.Response.Make(Cohttp_posix_io.Buffered_IO)

let colon = Re_str.regexp "[:]"

let get_user_agent () = Sys.argv.(0)


(* Use HTTP to frame RPC messages *)
let http_rpc string_of_call response_of_string ?(srcstr="unset") ?(dststr="unset") url call =
	let uri = Uri.of_string (url ()) in
	let req = string_of_call call in

	let headers = Cohttp.Header.of_list [
		"User-agent", get_user_agent ();
		"content-length", string_of_int (String.length req);
	] in
	(* If we have a username:password@ then use basic authentication *)
	let userinfo = Uri.userinfo uri in
	let headers = match userinfo with
		| Some x ->
			begin match Re_str.split_delim colon x with
			| username :: password :: [] ->
				Cohttp.Header.add_authorization headers (Cohttp.Auth.Basic (username, password))
			| _ -> headers
			end
		| None -> headers in


	let http_req = Cohttp.Request.make ~meth:`POST ~version:`HTTP_1_1 ~headers uri in

	Open_uri.with_open_uri uri
		(fun fd ->
			let ic = Unix.in_channel_of_descr fd in
			let oc = Unix.out_channel_of_descr fd in
			Request.write (fun t oc -> Request.write_body t oc req) http_req oc;
			match Response.read ic with
				| Some t ->
					begin match Cohttp.Response.status t with
						| `OK ->
							let body = match Response.read_body_chunk t ic with
							| Cohttp.Transfer.Chunk body
							| Cohttp.Transfer.Final_chunk body -> body
							| _ -> "" in
							response_of_string body
						| bad -> failwith (Printf.sprintf "Unexpected HTTP response code: %s" (Cohttp.Code.string_of_status bad))
					end
				| _ -> failwith (Printf.sprintf "Failed to read HTTP response from: %s" (url ()))
		)
let xml_http_rpc = http_rpc Xmlrpc.string_of_call Xmlrpc.response_of_string

