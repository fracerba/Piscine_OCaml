let () =
	let add_coefficient n =
		if n > 1 then
			string_of_int n ^ " "
		else
			""
	in let print_list lst =
		print_string (String.concat " + " (List.map (fun (m, n) -> add_coefficient n ^ m#formula) lst))
	in let print_reaction reaction =
		print_list (reaction#get_start);
		print_string " --> ";
		print_list (reaction#get_result);
		print_endline ("\nBalanced: " ^ string_of_bool reaction#is_balanced ^ "\n");
	in let test_reaction reaction =
		try
			print_reaction reaction;
		with Reaction.Unbalanced err -> begin 
			prerr_endline err;
			try
				let reaction_balanced = reaction#balance in
				print_reaction reaction_balanced;
			with Reaction.Unbalanced err -> prerr_endline err;
		end
	in let combustion_1 = new Reaction.alkane_combustion [new Alkane.methane] in
	let combustion_2 = new Reaction.alkane_combustion [new Alkane.ethane] in
	let combustion_3 = new Reaction.alkane_combustion [new Alkane.propane] in
	let combustion_4 = new Reaction.alkane_combustion [new Alkane.butane] in
	let combustion_5 = new Reaction.alkane_combustion [new Alkane.pentane] in
	let combustion_6 = new Reaction.alkane_combustion [new Alkane.hexane] in 
	let combustion_7 = new Reaction.alkane_combustion [new Alkane.heptane] in
	let combustion_8 = new Reaction.alkane_combustion [new Alkane.octane] in

	let combustions = [combustion_1; combustion_2; combustion_3; combustion_4; combustion_5; combustion_6; combustion_7; combustion_8] in
	List.iter test_reaction combustions;
	print_endline "\n\n";

	let combustion_9 = new Reaction.alkane_combustion [new Alkane.methane; new Alkane.methane] in
	let combustion_10 = new Reaction.alkane_combustion [new Alkane.methane; new Alkane.methane; new Alkane.methane] in
	let combustion_11 = new Reaction.alkane_combustion [new Alkane.ethane; new Alkane.ethane] in
	let combustion_12 = new Reaction.alkane_combustion [new Alkane.ethane; new Alkane.methane] in
	let combustion_13 = new Reaction.alkane_combustion [new Alkane.ethane; new Alkane.methane; new Alkane.propane] in
	let combustion_14 = new Reaction.alkane_combustion [new Alkane.ethane; new Alkane.butane] in
	let combustion_15 = new Reaction.alkane_combustion [new Alkane.pentane; new Alkane.hexane] in
	let combustion_16 = new Reaction.alkane_combustion [new Alkane.methane; new Alkane.ethane; new Alkane.propane; new Alkane.butane; new Alkane.pentane; 
		new Alkane.hexane; new Alkane.heptane; new Alkane.octane] in
	List.iter test_reaction [combustion_9; combustion_10; combustion_11; combustion_12; combustion_13; combustion_14; combustion_15; combustion_16]