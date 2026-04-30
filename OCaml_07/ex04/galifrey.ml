class galifrey (peoples : People.people list) (doctors : Doctor.doctor list) (daleks : Dalek.dalek list) =
	object (self)
		val daleks : Dalek.dalek list = daleks
		val doctors : Doctor.doctor list = doctors
		val people : People.people list = peoples

		method private list_iter f lst =
			List.iter f (List.rev lst#get_members);
			print_newline ()

		method private extract_shield (d : Dalek.dalek) =
			let s = d#to_string in
			let ridx = String.rindex s ':' in
			bool_of_string (String.sub s (ridx + 2) (String.length s - ridx - 2))

		method private build_army lst species : 'a Army.army =
			let extract_name c =
				let s = c#to_string in
				let idx = String.index s ':' in
				String.sub s (idx + 2) (String.index s '|' - idx - 3)
			in let print h =
				print_endline (extract_name h ^ " has joined the " ^ species ^ " army!");
			in let rec loop army lst =
				match lst with
					| [] -> (print_newline (); army)
					| h :: t -> (print h; loop (army#add h) t)
			in loop (new Army.army []) lst

		method private build_people_army lst species : People.people Army.army =
			let extract_name c =
				let s = c#to_string in
				let idx = String.index s ':' in
				String.sub s (idx + 2) (String.index s '|' - idx - 3)
			in let print h =
				print_endline (extract_name h ^ " has joined the " ^ species ^ " army!");
			in let rec loop army lst =
				match lst with
					| [] -> (print_newline (); army)
					| h :: t -> (print h; loop (army#add h) t)
			in loop (new Army.army []) lst

		method private build_doctors_army lst species : Doctor.doctor Army.army =
			let extract_name c =
				let s = c#to_string in
				let idx = String.index s ':' in
				String.sub s (idx + 2) (String.index s '|' - idx - 3)
			in let print h =
				print_endline (extract_name h ^ " has joined the " ^ species ^ " army!");
			in let rec loop army lst =
				match lst with
					| [] -> (print_newline (); army)
					| h :: t -> (print h; loop (army#add h) t)
			in loop (new Army.army []) lst

		method private build_daleks_army lst species : Dalek.dalek Army.army =
			let extract_name c =
				let s = c#to_string in
				let idx = String.index s ':' in
				String.sub s (idx + 2) (String.index s '|' - idx - 3)
			in let print h =
				print_endline (extract_name h ^ " has joined the " ^ species ^ " army!");
			in let rec loop army lst =
				match lst with
					| [] -> (print_newline (); army)
					| h :: t -> (print h; loop (army#add h) t)
			in loop (new Army.army []) lst

		method private destroy_army army f (species : string) : unit =
			match army#get_members with
				| [] -> print_endline ("The " ^ species ^ " army is completely destroyed!\n")
				| h :: t -> f h;
					self#destroy_army (army#delete) f species

		method private recreate_army (lst : 'a list) : 'a Army.army =
			List.fold_left (fun acc p -> acc#add p) (new Army.army []) lst

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
				if n < 0 then begin
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
				if Random.bool () then begin
					let doc = List.nth lst (Random.int (List.length lst)) in
					print_endline ("Exterminate!");
					List.map (damage doc) lst
				end
				else
					lst
			in let rec loop lst n =
				if n < 0 then begin
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
				if Random.bool () then begin
					let p = List.nth lst (Random.int (List.length lst)) in
					let dal = List.nth daleks#get_members n in
					print_endline ("Exterminate!");
					dal#exterminate p;
					List.filter (fun x -> x#to_string <> p#to_string) lst
				end
				else
					lst
			in let rec loop lst n =
				if n < 0 then begin
					print_newline ();
					recreate_army lst
				end
				else
					loop (exterminate_people n lst) (n - 1)
			in loop (List.rev peoples#get_members) ((List.length daleks#get_members) - 1)
			
		method private check_alive (entities : 'a Army.army) =
			List.length entities#get_members <> 0

		method private print_armies (peoples : People.people Army.army) (doctors : Doctor.doctor Army.army) (daleks : Dalek.dalek Army.army) =
			print_endline "Peoples:";
			self#list_iter (fun p -> print_endline (p#to_string)) peoples;
			print_endline "Doctors:";
			self#list_iter (fun d -> print_endline (d#to_string)) doctors;
			print_endline "Daleks:";
			self#list_iter (fun d -> print_endline (d#to_string)) daleks

		method private simulate_war (peoples : People.people Army.army) (doctors : Doctor.doctor Army.army) (daleks : Dalek.dalek Army.army) =
			if not (self#check_alive peoples) || not (self#check_alive doctors) then
				print_endline "The Daleks have won the Time War!"
			else if not (self#check_alive daleks) then
				print_endline "The Doctors and Peoples have won the Time War!"
			else begin
				self#print_armies peoples doctors daleks;
				if Random.bool () then begin
					print_endline "The Doctors are attacking the Daleks!";
					self#simulate_war peoples doctors (self#attack_daleks doctors daleks)
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
			let peoples_army = self#build_people_army people "people" in
			let doctors_army = self#build_doctors_army doctors "doctors" in
			let daleks_army = self#build_daleks_army daleks "daleks" in
			print_endline "The Time War has begun!";
			self#simulate_war peoples_army doctors_army daleks_army
end