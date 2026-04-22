module Set = Set.Set

let () =
	let print_set s f =
		print_string "{ ";
		Set.foreach s (fun x -> print_string ((f x) ^ "; "));
		print_endline "}"
	in let print_set_fun s f str =
		print_endline ("Applying function: \"" ^ str ^ "\"");
		print_set s f;
		print_newline ();
	in let print_bool set str =
		print_endline ("Applying function: \"" ^ str ^ "\"");
		print_endline (string_of_bool set);
		print_newline ();
	in let print_set_lst to_str lst =
		List.iter (fun x -> print_set x to_str) lst;
		print_newline ()
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
		let rec loop_bool lst set =
			match lst with
				| [] -> ()
				| (f, str) :: t -> (print_bool (f set) str; loop_bool t set)
		in let rec loop_set f_lst set =
			match f_lst with
				| [] -> loop_bool f_lst2 set
				| (f, str, prt) :: t -> (print_set_fun (f set) prt str; loop_set t (f set))
		in loop_set f_lst set
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
	print_newline ();

	let int_set1 =
		create_set int_set_list1 0
	and int_set2 = 
		create_set int_set_list2 0
	in

	print_endline "Int Set 1:";
	print_set int_set1 string_of_int;
	print_endline "\nInt Set 2:";
	print_set int_set2 string_of_int;
	print_newline ();

	let int_set_fun = [
		((fun x -> Set.bind x (fun x -> Set.return (x * 2))), "x * 2", string_of_int);
		((fun x -> Set.bind x (fun x -> Set.return (x * x))), "x * x", string_of_int);
		((fun x -> Set.union x int_set2), "union with Int Set 2", string_of_int);
		((fun x -> Set.union int_set2 x), "union with Int Set 2", string_of_int);
		((fun x -> Set.inter x int_set2), "intersection with Int Set 2", string_of_int);
		((fun x -> Set.inter int_set2 x), "intersection with Int Set 2", string_of_int);
		((fun x -> Set.diff x int_set2), "difference with Int Set 2", string_of_int);
		((fun x -> Set.diff int_set2 x), "difference with Int Set 2", string_of_int);
		((fun x -> Set.filter x (fun x -> x mod 2 = 0)), "filter even numbers", string_of_int);
		((fun x -> Set.filter x (fun x -> x < 7)), "filter if x < 7", string_of_int);
	]
	and int_set_fun2 = [
		((fun x -> Set.for_all x (fun x -> x < 5)), "for_all numbers less than 5");
		((fun x -> Set.for_all x (fun x -> x mod 2 = 0)), "for_all even numbers");
		((fun x -> Set.exists x (fun x -> x < 5)), "exists numbers less than 5");
		((fun x -> Set.exists x (fun x -> x mod 2 = 0)), "exists even numbers");
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
	print_endline "\nString Set 2:";
	print_set str_set2 (fun x -> x);
	print_newline ();

	let str_set_fun = [
		((fun x -> Set.bind x (fun x -> Set.return (x ^ x))), "x ^ x", (fun x -> x));
		((fun x -> Set.bind x (fun x -> Set.return (String.sub x 0 (String.length x / 2)))), "substring of first half", (fun x -> x));
		((fun x -> Set.union x str_set2), "union with String Set 2", (fun x -> x));
		((fun x -> Set.union str_set2 x), "union with String Set 2", (fun x -> x));
		((fun x -> Set.inter x str_set2), "intersection with String Set 2", (fun x -> x));
		((fun x -> Set.inter str_set2 x), "intersection with String Set 2", (fun x -> x));
		((fun x -> Set.diff x str_set2), "difference with String Set 2", (fun x -> x));
		((fun x -> Set.diff str_set2 x), "difference with String Set 2", (fun x -> x));
		((fun x -> Set.filter x (fun x -> String.length x > 5)), "filter strings with length > 5", (fun x -> x));
	]
	and str_set_fun2 = [
		((fun x -> Set.for_all x (fun x -> String.length x > 5)), "for_all strings with length > 5");
		((fun x -> Set.for_all x (fun x -> String.index x 'a' < 5)), "for_all strings with 'a' at index < 5");
		((fun x -> Set.exists x (fun x -> String.length x > 5)), "exists strings with length > 5");
		((fun x -> Set.exists x (fun x -> String.index x 'a' < 5)), "exists strings with 'a' at index < 5");
	] in print_fun_lst str_set1 str_set_fun str_set_fun2