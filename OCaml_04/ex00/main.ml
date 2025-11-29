let print_color c =
	print_endline ("Color.toString: " ^ (Color.toString c));
	print_endline ("Color.toStringVerbose: " ^ (Color.toStringVerbose c));
	print_newline ();

let () =
	let rec loop l =
		match l with
			| [] -> ()
			| h :: t -> print_color h;
									loop t
	in loop Color.all