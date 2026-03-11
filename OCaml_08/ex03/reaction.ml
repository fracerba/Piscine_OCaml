class virtual reaction (start : (Molecule.molecule * int) list) (result : (Molecule.molecule * int) list) =
	object (self)
		method virtual get_start : (Molecule.molecule * int) list
		method virtual get_result : (Molecule.molecule * int) list
		method virtual balance : reaction
		method virtual is_balanced : bool

		method private sort_start : (Molecule.molecule * int) list = List.sort (fun (a, _) (b, _) -> compare a#formula b#formula) start
		method private sort_result : (Molecule.molecule * int) list = List.sort (fun (a, _) (b, _) -> compare a#formula b#formula) result
		method private check_balance : bool =
			let get_symbols mol =
				List.map (fun a -> a#symbol) mol#atoms
			in let rec get_atoms mol acc n =
				if n <= 0 then
					acc
				else
					get_atoms mol (mol @ acc) (n - 1)
			in let rec get_atoms_list lst acc =
				match lst with
					| [] -> List.sort compare acc
					| (m, n) :: t -> get_atoms_list t ((get_atoms (get_symbols m) [] n) @ acc)
			in get_atoms_list self#sort_start [] = get_atoms_list self#sort_result []
	end