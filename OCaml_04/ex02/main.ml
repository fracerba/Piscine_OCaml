let () =
	let print_value c =
		print_string "toString: ";
		print_endline (Card.toString c);
		print_string "toStringVerbose: ";
		print_endline (Card.toStringVerbose c);
		print_char '\n';
	in let rec loop l =
		match l with
			| [] -> ()
			| h :: t -> print_value h;
									loop t
	in loop Card.all