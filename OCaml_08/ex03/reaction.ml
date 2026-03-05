class virtual reaction (start : (Molecule.molecule * int) list) (result : (Molecule.molecule * int) list) =
	object (self)
		val reaction_start : (Molecule.molecule * int) list = List.sort (fun (a, n) (b, m) -> compare a b) start
		val reaction_result : (Molecule.molecule * int) list = List.sort (fun (a, n) (b, m) -> compare a b) result

		method virtual get_start : (Molecule.molecule * int) list = reaction_start
		method virtual get_result : (Molecule.molecule * int) list = reaction_result
		method virtual balance : reaction
		method virtual is_balanced : bool =
			let rec count_molecules mol acc n =
				if n < 0 then
					acc
				else
					mol#formula :: acc
			in let get_list_mol lst acc =
				match lst with
					| [] -> acc
					| (m, n) :: t -> get_list_mol t (acc @ (count_molecules m [] (n - 1)))
			in let rec get_atoms mol =
				
			in get_atoms self#get_start = get_atoms self#get_result
	end

