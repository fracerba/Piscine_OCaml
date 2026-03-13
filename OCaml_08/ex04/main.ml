let () =
	let add_coefficient n =
		if n > 1 then
			string_of_int n ^ " "
		else
			""
	in let print_list lst =
		print_string (String.concat " + " (List.map (fun (m, n) -> add_coefficient n ^ m#formula) lst))
	in let print_reaction reaction =
		if reaction#get_start = [] then
			print_endline "No reaction"
		else begin
			print_list (reaction#get_start);
			print_string " --> ";
			print_list (reaction#get_result);
			print_endline ("\nBalanced: " ^ string_of_bool reaction#is_balanced)
		end
	in let test_reaction reaction =
		try
			print_reaction reaction;
			try
				let reaction_balanced = reaction#balance in
				if not (reaction#equals reaction_balanced) then begin
					print_endline "Smallest possible stoichiometric coefficients: ";
					print_reaction reaction_balanced
				end;
				print_newline ()
			with Reaction.Unbalanced err -> prerr_endline err;
		with Reaction.Unbalanced err -> begin 
			prerr_endline err;
			try
				let reaction_balanced = reaction#balance in
				print_reaction reaction_balanced;
				print_newline ()
			with Reaction.Unbalanced err -> prerr_endline err;
		end
	in let combustion_1 = new Reaction.alkane_combustion (new Alkane.methane#to_list 1) in
	let combustion_2 = new Reaction.alkane_combustion (new Alkane.ethane#to_list 1) in
	let combustion_3 = new Reaction.alkane_combustion (new Alkane.propane#to_list 1) in
	let combustion_4 = new Reaction.alkane_combustion (new Alkane.butane#to_list 1) in
	let combustion_5 = new Reaction.alkane_combustion (new Alkane.pentane#to_list 1) in
	let combustion_6 = new Reaction.alkane_combustion (new Alkane.hexane#to_list 1) in 
	let combustion_7 = new Reaction.alkane_combustion (new Alkane.heptane#to_list 1) in
	let combustion_8 = new Reaction.alkane_combustion (new Alkane.octane#to_list 1) in

	let combustions = [combustion_1; combustion_2; combustion_3; combustion_4; combustion_5; combustion_6; combustion_7; combustion_8] in
	List.iter test_reaction combustions;
	print_endline "\n";

	let combustion_9 = new Reaction.alkane_combustion (new Alkane.methane#to_list 2) in
	let combustion_10 = new Reaction.alkane_combustion (new Alkane.methane#to_list 3) in
	let combustion_11 = new Reaction.alkane_combustion (new Alkane.ethane#to_list 2) in
	let combustion_12 = new Reaction.alkane_combustion (new Alkane.ethane#to_list 1 @ new Alkane.methane#to_list 3 @ new Alkane.ethane#to_list 2) in
	let combustion_13 = new Reaction.alkane_combustion (new Alkane.ethane#to_list 2 @ new Alkane.methane#to_list 1 @ new Alkane.propane#to_list 1) in
	let combustion_14 = new Reaction.alkane_combustion (new Alkane.ethane#to_list 3 @ new Alkane.butane#to_list 5) in
	let combustion_15 = new Reaction.alkane_combustion (new Alkane.pentane#to_list 6 @ new Alkane.hexane#to_list 4) in
	let combustion_16 = new Reaction.alkane_combustion (new Alkane.methane#to_list 1 @ new Alkane.ethane#to_list 1 @ new Alkane.propane#to_list 1 @ 
		new Alkane.butane#to_list 1 @ new Alkane.pentane#to_list 1 @ new Alkane.hexane#to_list 1 @ new Alkane.heptane#to_list 1 @ new Alkane.octane#to_list 1) in
	List.iter test_reaction [combustion_9; combustion_10; combustion_11; combustion_12; combustion_13; combustion_14; combustion_15; combustion_16]