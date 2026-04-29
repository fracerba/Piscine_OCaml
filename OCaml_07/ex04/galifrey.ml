class galifrey (peoples : People.people list) (doctors : Doctor.doctor list) (daleks : Dalek.dalek list) =
	object (self)
		val daleks : Dalek.dalek list = daleks
		val doctors : Doctor.doctor list = doctors
		val people : People.people list = peoples

		method private list_iter f lst =
			List.iter f (List.rev lst#get_members);
			print_newline ()

		method private extract_name (s : string) =
			String.sub s (String.index s ':' + 2) (String.index s '|' - String.index s ':' - 3)

		method private extract_shield (s : string) =
			bool_of_string (String.sub s (String.rindex s ':' + 2) (String.length s - String.rindex s ':' - 2))

		method private build_army army lst species =
			match lst with
				| [] -> (print_newline (); army)
				| h :: t -> print_endline (self#extract_name (h#to_string) ^ " has joined the " ^ species ^ " army!");
					self#build_army (army#add h) t species

		method private destroy_army army f (species : string) =
			match army#get_members with
				| [] -> print_endline ("The " ^ species ^ " army is completely destroyed!\n")
				| h :: t -> f h;
					self#destroy_army (army#delete) f species

		method private recreate_army army lst =
			List.fold_left (fun acc p -> acc#add p) (new Army.army []) lst

		method private damage_doctor army doc =
			let damage d =
				if d#to_string = doc#to_string then
					d#take_damage (10 + Random.int 111)
				else
					d
			in let new_army =
				List.map damage (List.rev army#get_members)
			in self#recreate_army new_army

		method private exterminate_peoples_army peoples daleks =
			let exterminate_people n lst =
				if Random.bool () then begin
					let p = List.nth lst (Random.int (List.length lst)) in
					print_endline ("Exterminate!");
					(List.nth daleks#get_members n)#exterminate p;
					List.filter (fun x -> x#to_string <> p#to_string) lst
				end
				else
					lst
			in let rec loop lst n =
				if n < 0 then begin
					print_newline ();
					self#recreate_army lst
				end
				else
					loop (exterminate_people n lst) (n - 1)
			in loop (List.rev peoples#get_members) ((List.length daleks#get_members) - 1)
			
		method private check_alive (entities) =
			List.length entities#get_members <> 0

		method do_time_war = 
			print_endline "The Time War has begun!";
end