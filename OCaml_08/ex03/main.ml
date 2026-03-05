let () =
	let hydrogen = new Atom.hydrogen in
	let helium = new Atom.helium in
	let lithium = new Atom.lithium in
	let beryllium = new Atom.beryllium in
	let boron = new Atom.boron in
	let carbon = new Atom.carbon in
	let nitrogen = new Atom.nitrogen in
	let oxygen = new Atom.oxygen in

	let atoms = [hydrogen; helium; lithium; beryllium; boron; carbon; nitrogen; oxygen] in

	let water = new Molecule.water in
	let carbon_dioxide = new Molecule.carbon_dioxide in
	let ozone = new Molecule.ozone in
	let hydrogen_peroxide = new Molecule.hydrogen_peroxide in
	let ammonia = new Molecule.ammonia in
	let methane = new Molecule.methane in

	let molecules = [water; carbon_dioxide; ozone; hydrogen_peroxide; ammonia; methane] in

	let methane2 = new Alkane.methane in
	let ethane = new Alkane.ethane in
	let propane = new Alkane.propane in
	let butane = new Alkane.butane in
	let pentane = new Alkane.pentane in
	let hexane = new Alkane.hexane in

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