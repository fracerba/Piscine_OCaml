let () = 
	let water = new Molecule.water
	and carbon_dioxide = new Molecule.carbon_dioxide
	and dioxygen = new Molecule.dioxygen
	and ozone = new Molecule.ozone
	and hydrogen_peroxide = new Molecule.hydrogen_peroxide
	and ammonia = new Molecule.ammonia
	and methane = new Molecule.methane
	and hydrofluoric_acid = new Molecule.hydrofluoric_acid
	and hydrochloric_acid = new Molecule.hydrochloric_acid
	and sodium_chloride = new Molecule.sodium_chloride
	and sulfuric_acid = new Molecule.sulfuric_acid
	and chloric_acid = new Molecule.chloric_acid
	and trinitrotoluene = new Molecule.trinitrotoluene
	and benzene = new Molecule.benzene
	and glucose = new Molecule.glucose
	and fructose = new Molecule.fructose
	and chloroform = new Molecule.chloroform
	and carbon_monoxide = new Molecule.carbon_monoxide
	and soot = new Molecule.soot in

	let molecules = [water; carbon_dioxide; dioxygen; ozone; hydrogen_peroxide; ammonia; methane; hydrofluoric_acid; hydrochloric_acid; 
		sodium_chloride; sulfuric_acid; chloric_acid; trinitrotoluene; benzene; glucose; fructose; chloroform; carbon_monoxide; soot] in

	List.iter (fun m -> print_endline (m#name ^ " - " ^ m#formula)) molecules;
	print_newline ();

	List.iter (fun m -> print_endline (m#name ^ " - " ^ (String.concat ", " (List.map (fun a -> a#symbol) m#atoms)))) molecules;
	print_newline ();

	List.iter (fun m -> print_endline m#to_string) molecules;
	print_newline ();

	print_endline (string_of_bool (water#equals carbon_dioxide));
	print_endline (string_of_bool (glucose#equals fructose));
	print_endline (string_of_bool (glucose#equals new Molecule.glucose));
	print_newline ();

	let water2 = water#to_list 2
	and carbon_dioxide3 = carbon_dioxide#to_list 3
	and ozone4 = ozone#to_list 4 in
	List.iter (fun a -> print_endline a#to_string) water2;
	List.iter (fun a -> print_endline a#to_string) carbon_dioxide3;
	List.iter (fun a -> print_endline a#to_string) ozone4;