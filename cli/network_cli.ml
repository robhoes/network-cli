open Network_interface
open Network_client

let dbg = "cli"

let _ =
	let nics = Net.Interface.get_all dbg () in
	print_endline (String.concat ", " nics);
	Net.Bridge.create dbg "test" ();
	let nics = Net.Bridge.get_all dbg () in
	print_endline (String.concat ", " nics);
	()

