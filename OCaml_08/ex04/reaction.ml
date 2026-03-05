class virtual reaction (start : (Molecule.molecule * int) list) (result : (Molecule.molecule * int) list) =
	object (self)
		method virtual get_start : (Molecule.molecule * int) list
		method virtual get_result : (Molecule.molecule * int) list
		method virtual balance : reaction
		method virtual is_balanced : bool

		method private sort_start : (Molecule.molecule * int) list = List.sort (fun (a, _) (b, _) -> compare a#name b#name) start
		method private sort_result : (Molecule.molecule * int) list = List.sort (fun (a, _) (b, _) -> compare a#name b#name) result
		method private check_balance : bool =
			let rec get_atoms_molecule mol acc n =
				if n <= 0 then
					acc
				else
					get_atoms_molecule mol ((List.map (fun a -> a#symbol) mol#atoms) @ acc) (n - 1)
			in let rec get_atoms_list lst acc =
				match lst with
					| [] -> List.sort compare acc
					| (m, n) :: t -> get_atoms_list t ((get_atoms_molecule m [] n) @ acc)
			in get_atoms_list self#get_start [] = get_atoms_list self#get_result []
	end

class alkane_combustion (alkanes : Alkane.alkane list) =
	object
		inherit reaction ()
		method get_start : (Molecule.molecule * int) list = self#sort_start
		method get_result : (Molecule.molecule * int) list = self#sort_result
		method is_balanced : bool = self#check_balance
	end 