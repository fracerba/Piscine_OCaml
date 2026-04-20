let () =
	let print_hour a b sign r : unit =
		print_string (string_of_int a);
		print_string sign;
		print_string (string_of_int b);
		print_string " = ";
		print_endline (string_of_int r);
	in let print_hour_add (a : Watchtower.Watchtower.hour) (b : Watchtower.Watchtower.hour) : unit =
		print_hour a b " + " (Watchtower.Watchtower.add a b);
	and print_hour_sub (a : Watchtower.Watchtower.hour) (b : Watchtower.Watchtower.hour) : unit =
		print_hour a b " - " (Watchtower.Watchtower.sub a b);
	in let print_hour_lst print lst =
		let rec loop lst lst2 =
			match lst, lst2 with
				| a :: b, c :: d -> (print a c; loop (a :: b) d)
				| a :: b, [] -> loop b b
				| [], _ -> print_newline ()
		in loop lst lst
	in let hour1 : Watchtower.Watchtower.hour = 10
	and hour2 : Watchtower.Watchtower.hour = 4
	and hour3 : Watchtower.Watchtower.hour = 7 
	and hour4 : Watchtower.Watchtower.hour = Watchtower.Watchtower.zero in
	let hour_lst = [hour1; hour2; hour3; hour4] in

	print_endline "Watchtower hours:";
	print_endline ("zero: " ^ string_of_int Watchtower.Watchtower.zero);
	print_newline ();

	print_endline "Addition of hours:";
	print_hour_lst print_hour_add hour_lst;

	print_endline "Subtraction of hours:";
	print_hour_lst print_hour_sub hour_lst;