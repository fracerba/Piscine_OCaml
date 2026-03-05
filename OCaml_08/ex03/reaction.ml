class virtual reaction (start : (Molecule.molecule * int) list) (result : (Molecule.molecule * int) list) =
	object (self)
		val reaction_start : (Molecule.molecule * int) list = List.sort (fun (a, n) (b, m) -> compare a b) start
		val reaction_result : (Molecule.molecule * int) list = List.sort (fun (a, n) (b, m) -> compare a b) result

		method virtual get_start : (Molecule.molecule * int) list = reaction_start
		method virtual get_result : (Molecule.molecule * int) list = reaction_result
		method virtual balance : reaction
		method virtual is_balanced : bool =
			if get_atoms self#get_start = get_atoms self#get_result then
				true
			else
				false
	end

