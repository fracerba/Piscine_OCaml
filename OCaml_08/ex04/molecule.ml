class virtual molecule (nm : string) (atoms : Atom.atom list) =
	object (self)
		method name : string = nm
		method atoms : Atom.atom list = List.sort (fun a1 a2 -> compare a1#atomic_number a2#atomic_number) atoms
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
			in let rec to_string lst acc = 
				match lst with
					| [] -> acc
					| (a, n) :: t -> to_string t (acc ^ a ^ n)
			in let rec to_string_list lst acc =
				match lst with
					| [] -> to_string acc ""
					| (a, n) :: t -> to_string_list t (hill_order (a, n) acc)
			in let rec loop lst acc =
				match lst, acc with
					| [], _ -> List.sort (fun (a1, _) (a2, _) -> compare a1#symbol a2#symbol) acc
					| h :: t, (a, n) :: c when h#equals a -> loop t ((a, n + 1) :: c)
					| h :: t, _ -> loop t ((h, 1) :: acc)
			in to_string_list (loop self#atoms []) []
		method to_string : string = nm ^ " " ^ self#formula
		method equals (m : molecule) = self#name = m#name && self#formula = m#formula
		method to_list n =
			let rec aux acc n =
				if n <= 0 then 
					acc
				else
					aux (self :: acc) (n - 1)
			in aux [] n
	end

class water =
	object
		inherit molecule "Water" ((new Atom.hydrogen#to_list 2) @ (new Atom.oxygen#to_list 1))
	end

class carbon_dioxide =
	object
		inherit molecule "Carbon Dioxide" ((new Atom.carbon#to_list 1) @ (new Atom.oxygen#to_list 2))
	end

class dioxygen =
	object
		inherit molecule "Dioxygen" (new Atom.oxygen#to_list 2)
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
		inherit molecule "Glucose" ((new Atom.hydrogen#to_list 12) @ (new Atom.oxygen#to_list 6) @ (new Atom.carbon#to_list 6))
	end

class fructose =
	object
		inherit molecule "Fructose" ((new Atom.oxygen#to_list 6) @ (new Atom.carbon#to_list 6) @ (new Atom.hydrogen#to_list 12))
	end

class chloroform =
	object
		inherit molecule "Chloroform" ((new Atom.chlorine#to_list 3) @ (new Atom.carbon#to_list 1) @ (new Atom.hydrogen#to_list 1))
	end

class carbon_monoxide =
	object
		inherit molecule "Carbon Monoxide" ((new Atom.carbon#to_list 1) @ (new Atom.oxygen#to_list 1))
	end

class soot =
	object
		inherit molecule "Soot" (new Atom.carbon#to_list 1)
	end