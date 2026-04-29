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

		method private recreate_army lst =
			List.fold_left (fun acc p -> acc#add p) (new Army.army []) lst

		method private attack_daleks daleks doctors =
			let attack_dalek n lst =
				let d = List.nth lst (Random.int (List.length lst)) in
				if self#extract_shield (d#to_string) then begin
					(List.nth doctors#get_members n)#use_sonic_screwdriver;
					List.filter (fun x -> x#to_string <> d#to_string) lst
				end
				else
					lst
			in let rec loop lst n =
				if n < 0 then begin
					print_newline ();
					self#recreate_army lst
				end
				else
					loop (attack_dalek n lst) (n - 1)
			in loop (List.rev daleks#get_members) ((List.length doctors#get_members) - 1)

		method private damage_doctors_army doctors daleks =
			let damage d =
				if Random.bool () then begin
					print_endline ("Exterminate!");
					d#take_damage (10 + Random.int 111)
				end
				else
					d
			in self#recreate_army (List.map damage (List.rev doctors#get_members))

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

		method private print_armies peoples doctors daleks =
			print_endline "Peoples:";
			self#list_iter (fun p -> print_endline (p#to_string)) peoples;
			print_endline "Doctors:";
			self#list_iter (fun d -> print_endline (d#to_string)) doctors;
			print_endline "Daleks:";
			self#list_iter (fun d -> print_endline (d#to_string)) daleks

		method private simulate_war peoples doctors daleks =
			if not (self#check_alive peoples) || not (self#check_alive doctors) then
				print_endline "The Daleks have won the Time War!"
			else if not (self#check_alive daleks) then
				print_endline "The Doctors and Peoples have won the Time War!"
			else begin
				self#print_armies peoples doctors daleks;
				if Random.bool () then begin
					print_endline "The Doctors are attacking the Daleks!";
					self#simulate_war peoples doctors (self#attack_daleks daleks doctors)
				end
				else
					if Random.bool () then begin
						print_endline "The Daleks are attacking the Doctors!";
						self#simulate_war peoples (self#damage_doctors_army doctors daleks) daleks 
					end 
					else begin
						print_endline "The Daleks are attacking the Peoples!";
						self#simulate_war (self#exterminate_peoples_army peoples daleks) doctors daleks
					end
			end

		method do_time_war =
			let peoples_army = self#build_army (new Army.army []) people "people"
			and doctors_army = self#build_army (new Army.army []) doctors "doctors"
			and daleks_army = self#build_army (new Army.army []) daleks "daleks" in
			print_endline "The Time War has begun!";
			self#simulate_war peoples_army doctors_army daleks_army
end