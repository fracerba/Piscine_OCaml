module Try = Try.Try

let () =
	let print_try v f =
		match v with
			| Try.Success a -> print_endline ("Success: " ^ (f a))
			| Try.Failure a -> print_endline ("Failure: " ^ (Printexc.to_string a))
	in let print_try_int v =
		print_try v string_of_int
	and print_try_string v =
		print_try v (fun x -> x)
	in let print_try_try v f =
		match v with
			| Try.Success (Try.Success a) -> print_endline ("Success: (Success: " ^ (f a) ^ ")")
			| Try.Success (Try.Failure a) -> print_endline ("Success: (Failure: " ^ (Printexc.to_string a) ^ ")")
			| Try.Failure a -> print_endline ("Failure: " ^ (Printexc.to_string a))
	in let print_try_try_int v =
		print_try_try v string_of_int
	and print_try_try_string v =
		print_try_try v (fun x -> x)
	in let num1 = Try.return 10
	and num2 = Try.return 5
	and num3 = Try.return 0 in
	let lst = [num1; num2; num3] in
	List.iter print_try_int lst;
	print_newline ();

	let fun1 x = Try.bind x (fun x -> Try.return (20 / x)) in
	let lst = List.map (fun x -> fun1 x) lst in
	List.iter print_try_int lst;
	print_newline ();

	let fun2 x = Try.bind x (fun x -> Try.return (x * 2)) in
	let lst = List.map (fun x -> fun2 x) lst in
	List.iter print_try_int lst;
	print_newline ();

	let fun3 x = Try.recover x (fun e -> Try.return 0) in
	let lst = List.map (fun x -> fun3 x) lst in
	List.iter print_try_int lst;
	print_newline ();

	let fun4 x = Try.filter x (fun x -> x > 0) in
	let lst = List.map (fun x -> fun4 x) lst in
	List.iter print_try_int lst;
	print_newline ();

	let fun5 x = Try.return x in
	let lst = List.map (fun x -> fun5 x) lst in
	List.iter print_try_try_int lst;
	print_newline ();

	let fun6 x = Try.flatten x in
	let lst = List.map (fun x -> fun6 x) lst in
	List.iter print_try_int lst;
	print_endline "\n\n";

	let str1 = Try.return "Cesare"
	and str2 = Try.return "Pompeo"
	and str3 = Try.return "Crasso" in
	let lst = [str1; str2; str3] in
	List.iter print_try_string lst;
	print_newline ();

	let fun1 x = Try.bind x (fun x -> Try.return (String.index x 'o')) in
	let lst1 = List.map (fun x -> fun1 x) lst in
	List.iter print_try_int lst1;
	print_newline ();

	let fun2 x = Try.bind x (fun x -> Try.return (String.sub x 0 (String.index x 'r'))) in
	let lst = List.map (fun x -> fun2 x) lst in
	List.iter print_try_string lst;
	print_newline ();

	let fun3 x = Try.recover x (fun e -> Try.return "Augusto") in
	let lst = List.map (fun x -> fun3 x) lst in
	List.iter print_try_string lst;
	print_newline ();

	let fun4 x = Try.filter x (fun x -> String.index x 's' > 2) in
	let lst = List.map (fun x -> fun4 x) lst in
	List.iter print_try_string lst;
	print_newline ();

	let fun5 x = Try.return x in
	let lst = List.map (fun x -> fun5 x) lst in
	List.iter print_try_try_string lst;
	print_newline ();

	let fun6 x = Try.flatten x in
	let lst = List.map (fun x -> fun6 x) lst in
	List.iter print_try_string lst;