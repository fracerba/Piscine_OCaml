class galifrey =
	object (self)
		val daleks : Dalek.dalek list = []
		val doctors : Doctor.doctor list = []
		val people : People.people list = []

		method do_time_war = 
			print_endline "The Time War has begun!";
		method private list_iter f lst =
			List.iter f (List.rev lst#get_members);
			print_newline ()
		method private extract_name (s : string) =
			String.sub s 6 (String.index s '|' - 7)
		method private extract_shield (s : string) =
			bool_of_string (String.sub s (String.rindex s '|' + 10) (String.length s - String.rindex s '|' - 1))
		method private build_army army lst species =
			match lst with
				| [] -> print_newline ();
					army
				| h :: t -> print_endline (self#extract_name (h#to_string) ^ " has joined the " ^ species ^ " army!");
					self#build_army (army#add h) t species
		method private destroy_army army f (species : string) =
			match army#get_members with
				| [] -> print_endline ("The " ^ species ^ " army is completely destroyed!\n")
				| h :: t -> f h;
					self#destroy_army (army#delete) f species
		method private damage_doctors_army army =
			let rec loop lst acc =
				match lst with
					| [] -> print_newline ();
						acc
					| h :: t -> loop t (acc#add (h#take_damage (10 + Random.int 111)))
			in loop (List.rev army#get_members) (new Army.army [])
		method private exterminate_peoples_army peoples daleks =
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
		method private select_attack (attacker : 'a) (defender : 'b) = 
			
		method private check_alive (entities) =
			
end