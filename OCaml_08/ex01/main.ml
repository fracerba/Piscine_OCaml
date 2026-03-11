let () = 
	let water = new Molecule.water in
	let carbon_dioxide = new Molecule.carbon_dioxide in
	let dioxygen = new Molecule.dioxygen in
	let ozone = new Molecule.ozone in
	let hydrogen_peroxide = new Molecule.hydrogen_peroxide in
	let ammonia = new Molecule.ammonia in
	let methane = new Molecule.methane in
	let hydrofluoric_acid = new Molecule.hydrofluoric_acid in
	let hydrochloric_acid = new Molecule.hydrochloric_acid in
	let sodium_chloride = new Molecule.sodium_chloride in
	let sulfuric_acid = new Molecule.sulfuric_acid in
	let chloric_acid = new Molecule.chloric_acid in
	let trinitrotoluene = new Molecule.trinitrotoluene in
	let benzene = new Molecule.benzene in
	let glucose = new Molecule.glucose in
	let fructose = new Molecule.fructose in
	let chloroform = new Molecule.chloroform in

	let molecules = [water; carbon_dioxide; dioxygen; ozone; hydrogen_peroxide; ammonia; methane; hydrofluoric_acid; hydrochloric_acid; 
		sodium_chloride; sulfuric_acid; chloric_acid; trinitrotoluene; benzene; glucose; fructose; chloroform] in

	List.iter (fun m -> print_endline (m#name ^ " - " ^ m#formula)) molecules;
	print_newline ();

	List.iter (fun m -> print_endline (m#name ^ " - " ^ (String.concat ", " (List.map (fun a -> a#symbol) m#atoms)))) molecules;
	print_newline ();

	List.iter (fun m -> print_endline m#to_string) molecules;
	print_newline ();

	print_endline (string_of_bool (water#equals carbon_dioxide));
	print_endline (string_of_bool (glucose#equals fructose));
	print_endline (string_of_bool (glucose#equals new Molecule.glucose));