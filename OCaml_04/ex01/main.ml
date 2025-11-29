let print_value c =
	print_endline ("Value.toString: " ^ (Value.toString c));
	print_endline ("Value.toStringVerbose: " ^ (Value.toStringVerbose c));
	print_endline ("Value.toInt: " ^ (string_of_int (Value.toInt c)));
	print_string "Value.previous: ";
	if c <> Value.T2 then
		print_endline (Value.toString (Value.previous c))
	else
		print_endline "No previous for 2";
	print_string "Value.next: ";
	if c <> Value.As then
		print_endline (Value.toString (Value.next c))
	else
		print_endline "No next for As";
	print_newline ();

let () =
	let rec loop l =
		match l with
			| [] -> ()
			| h :: t -> print_value h;
									loop t
	in loop Value.all