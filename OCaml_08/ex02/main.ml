let () =
	let methane = new Alkane.methane in
	let ethane = new Alkane.ethane in
	let propane = new Alkane.propane in
	let butane = new Alkane.butane in
	let pentane = new Alkane.pentane in
	let hexane = new Alkane.hexane in
	let heptane = new Alkane.heptane in
	let octane = new Alkane.octane in
	let nonane = new Alkane.nonane in
	let decane = new Alkane.decane in
	let undecane = new Alkane.undecane in
	let dodecane = new Alkane.dodecane in
	let tridecane = new Alkane.tridecane in
	let tetradecane = new Alkane.tetradecane in
	let pentadecane = new Alkane.pentadecane in
	let hexadecane = new Alkane.hexadecane in

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

	let methane2 = methane#to_list 1 in
	let ethane2 = ethane#to_list 2 in
	let propane3 = propane#to_list 3 in
	List.iter (fun a -> print_endline a#to_string) methane2;
	List.iter (fun a -> print_endline a#to_string) ethane2;
	List.iter (fun a -> print_endline a#to_string) propane3;