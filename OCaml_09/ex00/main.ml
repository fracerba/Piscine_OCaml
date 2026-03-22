let () =
	let print_hour_add (a : Watchtower.hour) (b : Watchtower.hour) : unit =
		print_string (string_of_int a);
		print_string " + ";
		print_string (string_of_int b);
		print_string " = ";
		print_endline (string_of_int (Watchtower.add a b));
	and print_hour_sub (a : Watchtower.hour) (b : Watchtower.hour) : unit =
		print_string (string_of_int a);
		print_string " - ";
		print_string (string_of_int b);
		print_string " = ";
		print_endline (string_of_int (Watchtower.sub a b));
	in let hour1 : Watchtower.hour = 10
	and hour2 : Watchtower.hour = 4
	and hour3 : Watchtower.hour = Watchtower.zero 
	and hour4 : Watchtower.hour = 17 in

	print_endline "Watchtower hours:";
	print_endline ("zero: " ^ string_of_int Watchtower.zero);
	print_newline ();

	print_hour_add hour1 hour1;
	print_hour_add hour1 hour2;
	print_hour_add hour1 hour3;
	print_hour_add hour1 hour4;
	print_hour_add hour2 hour2;
	print_hour_sub hour2 hour3;
	print_hour_sub hour2 hour4;
	print_hour_add hour3 hour4;
	print_newline ();

	print_hour_sub hour1 hour1;
	print_hour_sub hour1 hour2;
	print_hour_sub hour1 hour3;
	print_hour_sub hour1 hour4;
	print_hour_sub hour2 hour2;
	print_hour_sub hour2 hour3;
	print_hour_sub hour2 hour4;
	print_hour_sub hour3 hour4;