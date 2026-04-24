module Set = Set.Set

let () =
	let print_set s f =
		print_string "{";
		Set.foreach s (fun x -> print_string ((f x) ^ "; "));
		print_endline "}\n"
	in let print_set_fun s f str =
		print_endline ("Applying function: " ^ str);
		print_set s f;
	in let print_bool set str =
		print_endline ("Applying function: " ^ str);
		print_endline (string_of_bool set);
		print_newline ();
	in let print_set_lst to_str lst =
		let print s f =
			print_string "{";
			Set.foreach s (fun x -> print_string ((f x)));
			print_string "}; "
		in print_string "[";
		List.iter (fun x -> print x to_str) lst;
		print_endline "]";
		print_newline ();
	in let lst_to_set_lst lst =
		List.map Set.return lst
	in let create_set lst elem =
		let rec loop lst acc =
			match lst with
				| [] -> acc
				| h :: t -> loop t (Set.union acc h)
		in match lst with
			| [] -> Set.return elem
			| h :: t -> loop t h
	in let print_fun_lst set f_lst f_lst2 =
		let rec loop_bool lst =
			match lst with
				| [] -> ()
				| (f, str) :: t -> (print_bool (f set) str; loop_bool t)
		in let rec loop_set f_lst =
			match f_lst with
				| [] -> loop_bool f_lst2
				| (f, str, prt) :: t -> (print_set_fun (f set) prt str; loop_set t)
		in loop_set f_lst
	in let int_set_list1 = 
		lst_to_set_lst [0; 1; 2; 3; 4; 5;]
	and int_set_list2 = 
		lst_to_set_lst [4; 5; 6; 7; 8; 9;]
	in 

	print_endline "Module Set with integers:";
	print_endline "Int Set list 1:";
	print_set_lst string_of_int int_set_list1;
	print_endline "Int Set list 2:";
	print_set_lst string_of_int int_set_list2;

	let int_set1 =
		create_set int_set_list1 0
	and int_set2 = 
		create_set int_set_list2 0
	in

	print_endline "Int Set 1:";
	print_set int_set1 string_of_int;
	print_endline "Int Set 2:";
	print_set int_set2 string_of_int;

	let int_set_fun = [
		((fun x -> Set.bind x (fun x -> Set.return (x * 2))), "Set.bind (n * 2)", string_of_int);
		((fun x -> Set.bind x (fun x -> Set.return (x * x))), "Set.bind (n * n)", string_of_int);
		((fun x -> Set.union x int_set2), "Set.union between Int Set 1 and Int Set 2", string_of_int);
		((fun x -> Set.union int_set2 x), "Set.union between Int Set 2 and Int Set 1", string_of_int);
		((fun x -> Set.inter x int_set2), "Set.intersection between Int Set 1 and Int Set 2", string_of_int);
		((fun x -> Set.inter int_set2 x), "Set.intersection between Int Set 2 and Int Set 1", string_of_int);
		((fun x -> Set.diff x int_set2), "Set.difference between Int Set 1 and Int Set 2", string_of_int);
		((fun x -> Set.diff int_set2 x), "Set.difference between Int Set 2 and Int Set 1", string_of_int);
		((fun x -> Set.filter x (fun x -> x mod 2 = 0)), "Set.filter (n % 2 = 0)", string_of_int);
		((fun x -> Set.filter x (fun x -> x < 4)), "Set.filter (n < 4)", string_of_int);
	]
	and int_set_fun2 = [
		((fun x -> Set.for_all x (fun x -> x >= 0)), "Set.for_all (n >= 0)");
		((fun x -> Set.for_all x (fun x -> x mod 2 = 0)), "Set.for_all (n % 2 = 0)");
		((fun x -> Set.exists x (fun x -> x = 0)), "Set.exists (n = 0)");
		((fun x -> Set.exists x (fun x -> x > 5)), "Set.exists (n > 5)");
	] in print_fun_lst int_set1 int_set_fun int_set_fun2;
	print_endline "\n";

	let str_set_list1 = 
		lst_to_set_lst ["Alpha"; "Beta"; "Gamma"; "Delta"; "Epsilon";]
	in let str_set_list2 = 
		lst_to_set_lst ["Delta"; "Epsilon"; "Zeta"; "Eta"; "Theta";]
	in 

	print_endline "Module Set with strings:";
	print_endline "String Set list 1:";
	print_set_lst (fun x -> x) str_set_list1;
	print_endline "String Set list 2:";
	print_set_lst (fun x -> x) str_set_list2;

	let str_set1 = 
		create_set str_set_list1 ""
	and str_set2 = 
		create_set str_set_list2 ""
	in

	print_endline "String Set 1:";
	print_set str_set1 (fun x -> x);
	print_endline "String Set 2:";
	print_set str_set2 (fun x -> x);

	let str_set_fun = [
		((fun x -> Set.bind x (fun x -> Set.return (x ^ x))), "Set.bind (str ^ str)", (fun x -> x));
		((fun x -> Set.bind x (fun x -> Set.return (String.sub x 0 (String.length x / 2)))), "Set.bind (substring of first half)", (fun x -> x));
		((fun x -> Set.union x str_set2), "Set.union between String Set 1 and String Set 2", (fun x -> x));
		((fun x -> Set.union str_set2 x), "Set.union between String Set 2 and String Set 1", (fun x -> x));
		((fun x -> Set.inter x str_set2), "Set.intersection between String Set 1 and String Set 2", (fun x -> x));
		((fun x -> Set.inter str_set2 x), "Set.intersection between String Set 2 and String Set 1", (fun x -> x));
		((fun x -> Set.diff x str_set2), "Set.difference between String Set 1 and String Set 2", (fun x -> x));
		((fun x -> Set.diff str_set2 x), "Set.difference between String Set 2 and String Set 1", (fun x -> x));
		((fun x -> Set.filter x (fun x -> String.length x > 5)), "Set.filter (length > 5)", (fun x -> x));
		((fun x -> Set.filter x (fun x -> String.exists (fun c -> c = 'a') x)), "Set.filter (string contains 'a')", (fun x -> x));
	]
	and str_set_fun2 = [
		((fun x -> Set.for_all x (fun x -> String.length x >= 4)), "Set.for_all (length >= 4)");
		((fun x -> Set.for_all x (fun x -> String.exists (fun c -> c = 'a') x)), "Set.for_all (string contains 'a')");
		((fun x -> Set.exists x (fun x -> String.length x = 7)), "Set.exists (length = 7)");
		((fun x -> Set.exists x (fun x -> String.exists (fun c -> c = 'f') x)), "Set.exists (string contains 'f')");
	] in print_fun_lst str_set1 str_set_fun str_set_fun2