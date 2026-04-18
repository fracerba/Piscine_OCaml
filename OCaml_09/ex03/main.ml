module Try = Try.Try

let () =
	let string_of_try v f =
		match v with
			| Try.Success a -> ("  Success: " ^ (f a))
			| Try.Failure a -> ("  Failure: " ^ (Printexc.to_string a))
	in let string_of_try_int v =
		string_of_try v string_of_int
	and string_of_try_string v =
		string_of_try v (fun x -> x)
	in let string_of_try_try v f =
		match v with
			| Try.Success (Try.Success a) -> ("  Success: (Success: " ^ (f a) ^ ")")
			| Try.Success (Try.Failure a) -> ("  Success: (Failure: " ^ (Printexc.to_string a) ^ ")")
			| Try.Failure a -> ("  Failure: " ^ (Printexc.to_string a))
	in let string_of_try_try_int v =
		string_of_try_try v string_of_int
	and string_of_try_try_string v =
		string_of_try_try v (fun x -> x)
	and print_try_lst to_str lst =
		List.iter (fun x -> print_endline (to_str x)) lst;
		print_newline ()
	in let print_fun (f, str, prt) lst = 
		print_endline ("Applying function: \"" ^ str ^ "\"");
		print_try_lst prt (List.map f lst)
	in let rec print_try f_lst e_lst f2 f3 =
		let print_try_try lst (f1, str1, prt1) (f2, str2, prt2) =
			print_fun (f1, str1, prt1) lst;
			let lst1 = List.map f1 lst in
			print_fun (f2, str2, prt2) lst1;
		in let rec loop f_lst e_lst =
			match f_lst with
				| [] -> print_try_try e_lst f2 f3
				| (f, str, prt) :: t -> (print_fun (f, str, prt) e_lst; loop t (List.map f e_lst))
		in loop f_lst e_lst
	in let int_lst = List.map Try.return [10; 5; 0] in

	print_endline "Module Try with integers:";
	print_try_lst string_of_try_int int_lst;

	let int_fun_lst = [ 
		((fun x -> Try.bind x (fun x -> Try.return (20 / x))), "20 / x", string_of_try_int);
		((fun x -> Try.bind x (fun x -> Try.return (x * 2))), "x * 2", string_of_try_int);
		((fun x -> Try.recover x (fun e -> Try.return 0)), "recover with 0", string_of_try_int);
		((fun x -> Try.filter x (fun x -> x > 0)), "filter if x > 0", string_of_try_int);
	]
	and int_fun2 =
		(Try.return, "return", string_of_try_try_int)
	and int_fun3 =
		(Try.flatten, "flatten", string_of_try_int)
	in print_try int_fun_lst int_lst int_fun2 int_fun3;
	print_endline "\n";

	let str_lst = List.map Try.return ["Cesare"; "Pompeo"; "Crasso"] in

	print_endline "Module Try with strings:";
	print_try_lst string_of_try_string str_lst;

	let str_fun1 =
		((fun x -> Try.bind x (fun x -> Try.return (String.index x 'o'))), "index of 'o'", string_of_try_int)
	and str_fun_lst = [ 
		((fun x -> Try.bind x (fun x -> Try.return (x ^ x))), "concatenate string with itself", string_of_try_string);
		((fun x -> Try.bind x (fun x -> Try.return (String.sub x 0 (String.index x 'r')))), "substring until 'r'", string_of_try_string);
		((fun x -> Try.recover x (fun e -> Try.return "Augusto")), "recover with \"Augusto\"", string_of_try_string);
		((fun x -> Try.filter x (fun x -> String.index x 's' > 2)), "filter if index of 's' > 2", string_of_try_string);
	]
	and str_fun2 =
		(Try.return, "return", string_of_try_try_string)
	and str_fun3 =
		(Try.flatten, "flatten", string_of_try_string)
	in print_fun str_fun1 str_lst;
	print_try str_fun_lst str_lst str_fun2 str_fun3;