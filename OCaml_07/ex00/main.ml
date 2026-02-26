let () =
	let people1 = new People.people "Rory Williams" and
	people2 = new People.people "Amy Pond" in
	print_newline ();

	print_endline people1#to_string;
	print_endline people2#to_string;
	print_newline ();

	people1#talk;
	people2#talk;
	print_newline ();

	people1#die;
	people2#die