let () =
	let methane = new Alkane.methane
	and ethane = new Alkane.ethane
	and propane = new Alkane.propane
	and butane = new Alkane.butane
	and pentane = new Alkane.pentane
	and hexane = new Alkane.hexane
	and heptane = new Alkane.heptane
	and octane = new Alkane.octane
	and nonane = new Alkane.nonane
	and decane = new Alkane.decane
	and undecane = new Alkane.undecane
	and dodecane = new Alkane.dodecane
	and tridecane = new Alkane.tridecane
	and tetradecane = new Alkane.tetradecane
	and pentadecane = new Alkane.pentadecane
	and hexadecane = new Alkane.hexadecane in

	let alkane_list = [methane; ethane; propane; butane; pentane; hexane; heptane; octane; nonane; 
		decane; undecane; dodecane; tridecane; tetradecane; pentadecane; hexadecane] in

	List.iter (fun a -> print_endline (a#name ^ " - " ^ a#formula)) alkane_list;
	print_newline ();

	List.iter (fun a -> print_endline (a#name ^ " - " ^ (String.concat ", " (List.map (fun a -> a#symbol) a#atoms)))) alkane_list;
	print_newline ();

	List.iter (fun a -> print_endline a#to_string) alkane_list;
	print_newline ();

	print_endline (string_of_bool (methane#equals new Alkane.methane));
	print_endline (string_of_bool (methane#equals new Molecule.methane));
	print_endline (string_of_bool (methane#equals ethane));
	print_newline ();

	let methane2 = methane#to_list 1
	and ethane2 = ethane#to_list 2
	and propane3 = propane#to_list 3 in
	List.iter (fun a -> print_endline a#to_string) methane2;
	List.iter (fun a -> print_endline a#to_string) ethane2;
	List.iter (fun a -> print_endline a#to_string) propane3;