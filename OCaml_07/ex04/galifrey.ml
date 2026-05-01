class galifrey (peoples : People.people list) (doctors : Doctor.doctor list) (daleks : Dalek.dalek list) =
	object (self)
		val daleks : Dalek.dalek list = daleks
		val doctors : Doctor.doctor list = doctors
		val people : People.people list = peoples

		method private extract_shield (d : Dalek.dalek) : bool =
			let s = d#to_string in
			let ridx = String.rindex s ':' in
			bool_of_string (String.sub s (ridx + 2) (String.length s - ridx - 2))

		method private build_people_army lst : People.people Army.army =
			let extract_name c =
				let s = c#to_string in
				let idx = String.index s ':' in
				String.sub s (idx + 2) (String.index s '|' - idx - 3)
			in let print h =
				print_endline (extract_name h ^ " has joined the People army!");
			in let rec loop army lst =
				match lst with
					| [] -> (print_newline (); army)
					| h :: t -> (print h; loop (army#add h) t)
			in loop (new Army.army []) lst

		method private build_doctors_army lst : Doctor.doctor Army.army =
			let extract_name c =
				let s = c#to_string in
				let idx = String.index s ':' in
				String.sub s (idx + 2) (String.index s '|' - idx - 3)
			in let print h =
				print_endline (extract_name h ^ " has joined the Doctor army!");
			in let rec loop army lst =
				match lst with
					| [] -> (print_newline (); army)
					| h :: t -> (print h; loop (army#add h) t)
			in loop (new Army.army []) lst

		method private build_daleks_army lst : Dalek.dalek Army.army =
			let extract_name c =
				let s = c#to_string in
				let idx = String.index s ':' in
				String.sub s (idx + 2) (String.index s '|' - idx - 3)
			in let print h =
				print_endline (extract_name h ^ " has joined the Dalek army!");
			in let rec loop army lst =
				match lst with
					| [] -> (print_newline (); army)
					| h :: t -> (print h; loop (army#add h) t)
			in loop (new Army.army []) lst

		method private attack_daleks (doctors : Doctor.doctor Army.army) (daleks : Dalek.dalek Army.army) : Dalek.dalek Army.army =
			let recreate_army lst =
				List.fold_left (fun acc p -> acc#add p) (new Army.army []) lst
			in let attack_dalek n lst =
				let dal = List.nth lst (Random.int (List.length lst)) in
				let doc = List.nth doctors#get_members n in
				doc#use_sonic_screwdriver;
				if not (self#extract_shield dal) then begin
					dal#die;
					List.filter (fun x -> x#to_string <> dal#to_string) lst
				end
				else
					lst
			in let rec loop lst n =
				if n < 0 || lst = [] then begin
					print_newline ();
					recreate_army lst
				end
				else
					loop (attack_dalek n lst) (n - 1)
			in loop (List.rev daleks#get_members) ((List.length doctors#get_members) - 1)

		method private damage_doctors_army (doctors : Doctor.doctor Army.army) (daleks : Dalek.dalek Army.army) : Doctor.doctor Army.army =
			let recreate_army lst =
				List.fold_left (fun acc p -> acc#add p) (new Army.army []) lst
			in let damage doc d =
				if d#to_string = doc#to_string then
					d#take_damage (10 + Random.int 111)
				else
					d
			in let damage_doctors lst =
				print_endline ("Exterminate!");
				if Random.bool () then begin
					let doc = List.nth lst (Random.int (List.length lst)) in
					List.map (damage doc) lst
				end
				else
					lst
			in let rec loop lst n =
				if n < 0 || lst = [] then begin
					print_newline ();
					recreate_army lst
				end
				else
					loop (damage_doctors lst) (n - 1)
			in loop (List.rev doctors#get_members) ((List.length daleks#get_members) - 1)

		method private exterminate_peoples_army (peoples : People.people Army.army) (daleks : Dalek.dalek Army.army) : People.people Army.army =
			let recreate_army lst =
				List.fold_left (fun acc p -> acc#add p) (new Army.army []) lst
			in let exterminate_people n lst =
				print_endline ("Exterminate!");
				if Random.bool () then begin
					let p = List.nth lst (Random.int (List.length lst)) in
					let dal = List.nth daleks#get_members n in
					dal#exterminate p;
					List.filter (fun x -> x#to_string <> p#to_string) lst
				end
				else
					lst
			in let rec loop lst n =
				if n < 0 || lst = [] then begin
					print_newline ();
					recreate_army lst
				end
				else
					loop (exterminate_people n lst) (n - 1)
			in loop (List.rev peoples#get_members) ((List.length daleks#get_members) - 1)
			
		method private check_people (entities : People.people Army.army) =
			List.length entities#get_members <> 0

		method private check_doctors (entities : Doctor.doctor Army.army) =
			List.length entities#get_members <> 0
		
		method private check_daleks (entities : Dalek.dalek Army.army) =
			List.length entities#get_members <> 0

		method private print_armies (peoples : People.people Army.army) (doctors : Doctor.doctor Army.army) (daleks : Dalek.dalek Army.army) =
			let print_species species lst =
				if lst <> [] then begin
					print_endline (species ^ ":");
					List.iter (fun p -> print_endline (p#to_string)) lst;
					print_newline ()
				end
			in let people_list = List.rev peoples#get_members in
			let doctor_list = List.rev doctors#get_members in
			let dalek_list = List.rev daleks#get_members in

			if people_list <> [] then
				print_species "Peoples" people_list;
			if doctor_list <> [] then
				print_species "Doctors" doctor_list;
			if dalek_list <> [] then
				print_species "Daleks" dalek_list;

		method private simulate_war (peoples : People.people Army.army) (doctors : Doctor.doctor Army.army) (daleks : Dalek.dalek Army.army) =
			self#print_armies peoples doctors daleks;
			if not (self#check_people peoples) || not (self#check_doctors doctors) then
				print_endline "The Daleks have won the Time War!"
			else if not (self#check_daleks daleks) then
				print_endline "The Doctors and Peoples have won the Time War!"
			else begin
				let shield = List.for_all self#extract_shield daleks#get_members in
				if Random.bool () && not shield then begin
					print_endline "The Doctors are attacking the Daleks!";
					self#simulate_war peoples doctors (self#attack_daleks doctors daleks)
				end
				else
					if Random.bool () && not shield then begin
						print_endline "The Daleks are attacking the Doctors!";
						self#simulate_war peoples (self#damage_doctors_army doctors daleks) daleks 
					end 
					else begin
						print_endline "The Daleks are attacking the Peoples!";
						self#simulate_war (self#exterminate_peoples_army peoples daleks) doctors daleks
					end
			end

		method do_time_war =
			let peoples_army = self#build_people_army people in
			let doctors_army = self#build_doctors_army doctors in
			let daleks_army = self#build_daleks_army daleks in
			print_endline "THE TIME WAR HAS BEGUN!\n";
			self#simulate_war peoples_army doctors_army daleks_army
	end