open Network_interface
open Network_client
open Cmdliner

let dbg = "cli"

let (|>) x f = f x

(* Interface commands *)

let iface_arg =
	let doc = "Interface name" in
	Arg.(required & pos 0 (some string) None & info [] ~docv:"INTERFACE" ~doc)

let list_iface () =
	let all = Net.Interface.get_all dbg () in
	List.iter print_endline all

let list_iface_cmd =
	let doc = "List all interfaces" in
	let man = [] in
	Term.(pure list_iface $ pure ()),
	Term.info "list-iface" ~doc ~man

let get_mac iface =
	try
		let mac = Net.Interface.get_mac dbg iface in
		`Ok (print_endline mac)
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_mac_cmd =
	let doc = "Get the MAC address of an interface" in
	let man = [] in
	Term.(ret (pure get_mac $ iface_arg)),
	Term.info "get-mac" ~doc ~man

let is_up iface =
	try
		let up = Net.Interface.is_up dbg iface in
		`Ok (print_endline (if up then "up" else "not up"))
	with _ ->
		`Error (false, iface ^ " is not an interface")

let is_up_cmd =
	let doc = "Check whether an interface is up or down" in
	let man = [] in
	Term.(ret (pure is_up $ iface_arg)),
	Term.info "is-up" ~doc ~man

let get_ipv4_addr iface =
	try
		let addrs = Net.Interface.get_ipv4_addr dbg iface in
		List.iter (fun (addr, prefix) ->
			Printf.printf "%s/%d\n" (Unix.string_of_inet_addr addr) prefix
		) addrs;
		`Ok ()
	with _ ->
		`Error (false, iface ^ " is not an interface")

let get_ipv4_addr_cmd =
	let doc = "Get IPv4 addresses (CIDRs) of an interface" in
	let man = [] in
	Term.(ret (pure get_ipv4_addr $ iface_arg)),
	Term.info "get-ipv4-addr" ~doc ~man

let set_ipv4_addr iface conf =
	try
		let conf' =
			if conf = "none" then
				None4
			else if conf = "dhcp" then
				DHCP4
			else
				let i = String.index conf '/' in
				let n = String.length conf in
				let addr = Unix.inet_addr_of_string (String.sub conf 0 i) in
				let prefix = String.sub conf (i + 1) (n - i - 1) |> int_of_string in
				Static4 [addr, prefix]
		in
		Net.Interface.set_ipv4_conf dbg iface conf';
		`Ok ()
	with _ ->
		`Error (false, "something went wrong")

let set_ipv4_addr_cmd =
	let doc = "Interface name (none|dhcp|<cidr>)" in
	let conf_arg = Arg.(required & pos 1 (some string) None & info [] ~docv:"IPV4-CONF" ~doc) in
	let doc = "Set IPv4 configuration of an interface" in
	let man = [] in
	Term.(ret (pure set_ipv4_addr $ iface_arg $ conf_arg)),
	Term.info "set-ipv4-addr" ~doc ~man

(* Bridge commands *)

let list_br () =
	let all = Net.Bridge.get_all dbg () in
	List.iter print_endline all

let list_br_cmd =
	let doc = "List all bridges" in
	let man = [] in
	Term.(pure list_br $ pure ()),
	Term.info "list-br" ~doc ~man

let default_cmd =
	let doc = "CLI for xcp-networkd" in
	let man = [] in
	Term.(ret (pure (`Help (`Pager, None)))),
	Term.info "network-cli" ~version:"0.1" ~doc ~man

let cmds = [ list_iface_cmd; get_mac_cmd; is_up_cmd; get_ipv4_addr_cmd; set_ipv4_addr_cmd;
	list_br_cmd ]

let _ =
	match Term.eval_choice default_cmd cmds with
	| `Error _ -> exit 1 | _ -> exit 0
