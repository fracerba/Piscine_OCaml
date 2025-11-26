let () =
	let print_value c =
		print_string "toString: ";
		print_endline (Value.toString c);
		print_string "toStringVerbose: ";
		print_endline (Value.toStringVerbose c);
		print_string "toInt: ";
		print_int (Value.toInt c);
		print_string "\nprevious: ";
		if c <> Value.T2 then
			print_endline (Value.toString (Value.previous c))
		else
			print_endline "No previous for 2";
		print_string "next: ";
		if c <> Value.As then
			print_endline (Value.toString (Value.next c))
		else
			print_endline "No next for As";
		print_newline ();
	in let rec loop l =
		match l with
			| [] -> ()
			| h :: t -> print_value h;
									loop t
	in loop Value.all