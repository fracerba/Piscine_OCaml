let extract_name (s : string) =
	String.sub s 6 (String.index s '|' - 7)

let list_iter f lst =
	List.iter f (List.rev lst#get_members);
	print_newline ()

let rec build_army army lst species =
	match lst with
		| [] -> print_newline ();
			army
		| h :: t -> print_endline (extract_name (h#to_string) ^ " has joined the " ^ species ^ " army!");
			build_army (army#add h) t species

let rec destroy_army army f (species : string) =
	match army#get_members with
		| [] -> print_endline ("The " ^ species ^ " army is completely destroyed!\n")
		| h :: t -> f h;
			destroy_army (army#delete) f species

let damage_doctors_army army =
	let rec loop lst acc =
		match lst with
			| [] -> print_newline ();
				acc
			| h :: t -> loop t (acc#add (h#take_damage (10 + Random.int 111)))
	in loop (List.rev army#get_members) (new Army.army [])

let exterminate_peoples_army peoples daleks =
	let exterminate_people p acc =
		if Random.bool () then begin
			print_endline ("Exterminate!");
			(List.nth daleks#get_members (Random.int (List.length daleks#get_members)))#exterminate p;
			acc
		end
		else
			acc#add p
	in let rec loop lst acc =
		match lst with
			| [] -> print_newline ();
				acc
			| h :: t -> loop t (exterminate_people h acc)
	in loop (List.rev peoples#get_members) (new Army.army [])

let () = 
	let peoples_names = ["Donna Noble"; "Amy Pond"; "Rory Williams"; "Rose Tyler"; "Clara Oswald";] in
	let peoples = List.init 5 (fun i -> new People.people (List.nth peoples_names i)) in
	print_newline ();

	let doctors_names = ["The Ninth Doctor"; "The Tenth Doctor"; "The Eleventh Doctor";] in
	let doctors = List.init 3 (fun i -> new Doctor.doctor (List.nth doctors_names i) (1200 + i * 300) (List.nth peoples i)) in
	print_newline ();
	let daleks = List.init 5 (fun _ -> new Dalek.dalek) in

	let peoples_army = build_army (new Army.army []) peoples "People" in
	let doctors_army = build_army (new Army.army []) doctors "Doctor" in
	let daleks_army = build_army (new Army.army []) daleks "Dalek" in

	list_iter (fun p -> print_endline (p#to_string)) peoples_army;
	list_iter (fun d -> print_endline (d#to_string)) doctors_army;
	list_iter (fun d -> print_endline (d#to_string)) daleks_army;

	list_iter (fun p -> p#talk) peoples_army;
	list_iter (fun d -> d#talk) doctors_army;
	list_iter (fun d -> d#talk) daleks_army;
	
	let doctors_army2 = damage_doctors_army doctors_army in
	list_iter (fun d -> print_endline (d#to_string)) doctors_army2;

	let peoples_army2 = exterminate_peoples_army peoples_army daleks_army in
	list_iter (fun p -> print_endline (p#to_string)) peoples_army2;
	list_iter (fun d -> print_endline (d#to_string)) daleks_army;

	list_iter (fun d -> d#use_sonic_screwdriver) doctors_army2;

	destroy_army daleks_army (fun d -> d#die) "Dalek";
	destroy_army peoples_army2 (fun p -> p#die) "People";
	destroy_army doctors_army2 (fun d -> d#travel_in_time (Random.int 10000) (Random.int 10000)) "Doctor";