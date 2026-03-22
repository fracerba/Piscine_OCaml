let () =
	let hydrogen = new Atom.hydrogen
	and helium = new Atom.helium
	and lithium = new Atom.lithium
	and beryllium = new Atom.beryllium
	and boron = new Atom.boron
	and carbon = new Atom.carbon
	and nitrogen = new Atom.nitrogen
	and oxygen = new Atom.oxygen in

	let atoms = [hydrogen; helium; lithium; beryllium; boron; carbon; nitrogen; oxygen] in

	let water = new Molecule.water
	and carbon_dioxide = new Molecule.carbon_dioxide
	and dioxygen = new Molecule.dioxygen
	and ozone = new Molecule.ozone
	and hydrogen_peroxide = new Molecule.hydrogen_peroxide
	and ammonia = new Molecule.ammonia
	and methane = new Molecule.methane in

	let molecules = [water; carbon_dioxide; dioxygen; ozone; hydrogen_peroxide; ammonia; methane] in

	let methane2 = new Alkane.methane
	and ethane = new Alkane.ethane
	and propane = new Alkane.propane
	and butane = new Alkane.butane
	and pentane = new Alkane.pentane
	and hexane = new Alkane.hexane in

	let alkane_list = [methane2; ethane; propane; butane; pentane; hexane] in

	List.iter (fun a -> print_endline a#to_string) atoms;
	print_newline ();

	List.iter (fun m -> print_endline m#to_string) molecules;
	print_newline ();

	List.iter (fun a -> print_endline a#to_string) alkane_list;
	print_newline ();

	print_endline (string_of_bool (methane2#equals methane2));
	print_endline (string_of_bool (methane2#equals methane));
	print_endline (string_of_bool (methane2#equals ethane));