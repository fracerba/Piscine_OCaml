let () = 
	let rec build_army (army : 'a Army.army) lst n =
		if n <= 0 then
			army
		else begin
			let new_soldier = List.nth lst n in
			print_endline (new_soldier#to_string ^ " has joined the army!");
			build_army (army#add new_soldier) lst (n - 1)
		end
	in let rec destroy_army_dalek (army : 'a Army.army) (species : string) =
		match army#get_members with
		| [] -> print_endline ("The " ^ species ^ " army is completely destroyed!")
		| h :: t ->
			h#die;
			destroy_army_dalek army#delete species
	in let peoples_army = new Army.army [] in
	let doctors_army = new Army.army [] in
	let daleks_army = new Army.army [] in
	let peoples_name = ["Donna Noble"; "Amy Pond"; "Clara Oswald"; "Rory Williams"; "Rose Tyler"; ] in
	let doctors_name = ["The Ninth Doctor"; "The Tenth Doctor"; "The Eleventh Doctor";] in
	let peoples = List.init 5 (fun i -> new People.people (List.nth peoples_name i)) in
	let doctors = List.init 3 (fun i -> new Doctor.doctor (List.nth doctors_name i) (1200 + i * 300) (List.nth peoples i)) in
	let daleks = List.init 5 (fun _ -> new Dalek.dalek) in
	let peoples_army = build_army peoples_army peoples 5 in
	let doctors_army = build_army doctors_army doctors 3 in
	let daleks_army = build_army daleks_army daleks 5 in
	destroy_army_dalek daleks_army "Dalek";
	(* destroy_army_dalek doctors_army "Doctor"; *)
	destroy_army_dalek peoples_army "People";