class virtual molecule (nm : string) (atoms : Atom.atom list) =
	object (self)
		val name : string = nm
		val atoms : Atom.atom list = atoms
		
		method name : string = name
		method formula : string =
			let count_atoms n =
				if n > 1 then 
					string_of_int n 
				else 
					""
			in let hill_order (a, n) acc =
				if a#symbol = "C" then
					(a#symbol, count_atoms n) :: acc
				else if a#symbol = "H" then
					match acc with
						| (h, m) :: t when h = "C" -> (h, m) :: (a#symbol, count_atoms n) :: t
						| h :: t -> acc @ [(a#symbol, count_atoms n)]
						| [] -> [(a#symbol, count_atoms n)]
				else
					acc @ [(a#symbol, count_atoms n)]
			in let rec to_string acc = 
				match acc with
					| [] -> ""
					| (a, n) :: t -> a ^ n ^ to_string t
			in let to_string_list l =
				let rec aux acc l =
					match l with
						| [] -> to_string acc
						| (a, n) :: t -> aux (hill_order (a, n) acc) t
				in aux [] l
			in let update a (b, n) =
				if a#equals b then
					(b, n + 1)
				else
					(b, n)
			in let rec loop lst acc =
				match lst with
					| [] -> List.sort (fun (a1, _) (a2, _) -> compare a1#symbol a2#symbol) acc
					| h :: t when (List.exists (fun (a, _) -> h#equals a) acc) -> loop t (List.map (update h) acc)
					| h :: t -> loop t ((h, 1) :: acc)
			in to_string_list (loop atoms [])
		method to_string : string = name ^ " " ^ self#formula
		method equals (m : molecule) = self#name = m#name && self#formula = m#formula
	end

class water =
	object
		inherit molecule "Water" ((new Atom.hydrogen#to_list 2) @ (new Atom.oxygen#to_list 1))
	end

class carbon_dioxide =
	object
		inherit molecule "Carbon Dioxide" ((new Atom.carbon#to_list 1) @ (new Atom.oxygen#to_list 2))
	end

class ozone =
	object
		inherit molecule "Ozone" (new Atom.oxygen#to_list 3)
	end

class hydrogen_peroxide =
	object
		inherit molecule "Hydrogen Peroxide" ((new Atom.hydrogen#to_list 2) @ (new Atom.oxygen#to_list 2))
	end

class ammonia =
	object
		inherit molecule "Ammonia" ((new Atom.nitrogen#to_list 1) @ (new Atom.hydrogen#to_list 3))
	end

class methane =
	object
		inherit molecule "Methane" ((new Atom.carbon#to_list 1) @ (new Atom.hydrogen#to_list 4))
	end

class hydrofluoric_acid =
	object
		inherit molecule "Hydrofluoric Acid" ((new Atom.hydrogen#to_list 1) @ (new Atom.fluorine#to_list 1))
	end

class hydrochloric_acid =
	object
		inherit molecule "Hydrochloric Acid" ((new Atom.hydrogen#to_list 1) @ (new Atom.chlorine#to_list 1))
	end

class sodium_chloride =
	object
		inherit molecule "Sodium Chloride" ((new Atom.sodium#to_list 1) @ (new Atom.chlorine#to_list 1))
	end

class sulfuric_acid =
	object
		inherit molecule "Sulfuric Acid" ((new Atom.sulfur#to_list 1) @ (new Atom.hydrogen#to_list 2) @ (new Atom.oxygen#to_list 4))
	end

class chloric_acid =
	object
		inherit molecule "Chloric Acid" ((new Atom.chlorine#to_list 1) @ (new Atom.hydrogen#to_list 1) @ (new Atom.oxygen#to_list 3))
	end

class trinitrotoluene =
	object
		inherit molecule "Trinitrotoluene" ((new Atom.nitrogen#to_list 3) @ (new Atom.hydrogen#to_list 5) @ (new Atom.oxygen#to_list 6) @ (new Atom.carbon#to_list 7))
	end

class benzene =
	object
		inherit molecule "Benzene" ((new Atom.carbon#to_list 6) @ (new Atom.hydrogen#to_list 6))
	end

class glucose =
	object
		inherit molecule "Glucose" ((new Atom.hydrogen#to_list 6) @ (new Atom.oxygen#to_list 6) @ (new Atom.carbon#to_list 6) @ (new Atom.hydrogen#to_list 6))
	end

class fructose =
	object
		inherit molecule "Fructose" ((new Atom.carbon#to_list 6) @ (new Atom.hydrogen#to_list 12) @ (new Atom.oxygen#to_list 6))
	end

class chloroform =
	object
		inherit molecule "Chloroform" ((new Atom.carbon#to_list 1) @ (new Atom.hydrogen#to_list 1) @ (new Atom.chlorine#to_list 3))
	end