let () =
	let print_color c =
		print_string "toString: ";
		print_endline (Color.toString c);
		print_string "toStringVerbose: ";
		print_endline (Color.toStringVerbose c);
	in let rec loop l =
		match l with
			| [] -> ()
			| h :: t -> print_color h;
									loop t
	in loop Color.all