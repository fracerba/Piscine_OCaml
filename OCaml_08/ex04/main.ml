let () =
	let add_coefficient n =
		if n > 1 then
			string_of_int n ^ " "
		else
			""
	in let print_balance b =
		print_endline ("  Balance: " ^ string_of_bool b)
	in let print_list lst =
		print_string (String.concat " + " (List.map (fun (m, n) -> add_coefficient n ^ m#formula) lst))
	in let sort_fun a b =
		compare (a#formula, a#name) (b#formula, b#name)
	in let rec get_alkane_list lst acc =
		match lst, acc with
			| [], _ -> List.sort (fun (a, _) (b, _) -> sort_fun a b) acc
			| h :: t, (m, n) :: c when h#equals m -> get_alkane_list t ((m, n + 1) :: c)
			| h :: t, _ -> get_alkane_list t ((h, 1) :: acc)
	in let get_sorted_list lst =
		get_alkane_list (List.sort (fun a b -> sort_fun a b) lst) []
	in let print_alkanes lst =
		print_endline "Alkanes list:";
		print_string "  ";
		print_list (get_sorted_list lst);
		print_newline ()
	in let print_reaction reaction =
		print_string "  ";
		if reaction#get_start = [] then
			print_endline "No reaction"
		else begin
			print_list (reaction#get_start);
			print_string " --> ";
			print_list (reaction#get_result);
			print_newline ();
			print_balance reaction#is_balanced
		end
	in let test_reaction lst =
		let reaction = new Reaction.alkane_combustion lst in
		print_alkanes lst;
		try
			print_endline "Reaction:";
			print_reaction reaction;
			try
				let reaction_balanced = reaction#balance in
				if not (reaction#equals reaction_balanced) then begin
					print_endline "Balanced reaction with smallest possible stoichiometric coefficients:";
					print_reaction reaction_balanced
				end;
				print_newline ()
			with Reaction.Unbalanced err -> prerr_endline err;
		with Reaction.Unbalanced err -> begin 
			print_endline err;
			print_balance reaction#is_balanced;
			try
				let reaction_balanced = reaction#balance in
				print_endline "Balanced reaction:";
				print_reaction reaction_balanced;
				print_newline ()
			with Reaction.Unbalanced err -> prerr_endline err;
		end
	in let combustion_1 = new Alkane.methane#to_list 1 in
	let combustion_2 = new Alkane.ethane#to_list 1 in
	let combustion_3 = new Alkane.propane#to_list 1 in
	let combustion_4 = new Alkane.butane#to_list 1 in
	let combustion_5 = new Alkane.pentane#to_list 1 in
	let combustion_6 = new Alkane.hexane#to_list 1 in 
	let combustion_7 = new Alkane.heptane#to_list 1 in
	let combustion_8 = new Alkane.octane#to_list 1 in

	let combustions = [combustion_1; combustion_2; combustion_3; combustion_4; combustion_5; combustion_6; combustion_7; combustion_8] in
	List.iter test_reaction combustions;
	print_endline "\n";

	let combustion_9 = new Alkane.methane#to_list 2 in
	let combustion_10 = new Alkane.methane#to_list 3 in
	let combustion_11 = new Alkane.ethane#to_list 2 in
	let combustion_12 = new Alkane.ethane#to_list 1 @ new Alkane.methane#to_list 3 @ new Alkane.ethane#to_list 2 in
	let combustion_13 = new Alkane.ethane#to_list 2 @ new Alkane.methane#to_list 1 @ new Alkane.propane#to_list 1 in
	let combustion_14 = new Alkane.ethane#to_list 3 @ new Alkane.butane#to_list 5 in
	let combustion_15 = new Alkane.pentane#to_list 6 @ new Alkane.hexane#to_list 4 in
	let combustion_16 = new Alkane.methane#to_list 1 @ new Alkane.ethane#to_list 1 @ new Alkane.propane#to_list 1 @ new Alkane.butane#to_list 1 @ 
		new Alkane.pentane#to_list 1 @ new Alkane.hexane#to_list 1 @ new Alkane.heptane#to_list 1 @ new Alkane.octane#to_list 1 in
	List.iter test_reaction [combustion_9; combustion_10; combustion_11; combustion_12; combustion_13; combustion_14; combustion_15; combustion_16]