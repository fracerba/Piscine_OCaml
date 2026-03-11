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

exception Unbalanced of string

class alkane_combustion (alkanes : Alkane.alkane list) =
	let rec parse lst acc =
		match lst, acc with
			| [], _ -> acc
			| h :: t, (m, n) :: c when h#equals m -> parse t ((m, n + 1) :: c)
			| h :: t, _ -> parse t ((h, 1) :: acc)
	in let count acc a b =
		if a#equals b then 
			acc + 1 
		else 
			acc
	in let rec count_atoms lst n atom =
		match lst with
			| [] -> n
			| h :: t -> count_atoms t (n + List.fold_left (fun acc a -> count acc a atom) 0 h#atoms) atom
	in let alkane_list : (Molecule.molecule * int) list =
		parse alkanes []
	in let carbon_nmb : int = 
		count_atoms alkanes 0 (new Atom.carbon)
	in let hydrogen_nmb : int = 
		count_atoms alkanes 0 (new Atom.hydrogen)
	in let start_list : (Molecule.molecule * int) list =
		alkane_list @ [(new Molecule.dioxygen, carbon_nmb + hydrogen_nmb / 4)]
	in let result_list : (Molecule.molecule * int) list =
		[(new Molecule.carbon_dioxide, carbon_nmb); (new Molecule.water, hydrogen_nmb / 2)]
	in object (self)
		inherit reaction start_list result_list
		method get_start : (Molecule.molecule * int) list =
			if self#is_balanced then
				self#sort_start
			else
				raise (Unbalanced "Reaction is not balanced")
		method get_result : (Molecule.molecule * int) list = 
			if self#is_balanced then
				self#sort_result
			else
				raise (Unbalanced "Reaction is not balanced")
		method balance = 
			if self#is_balanced then
				(self :> reaction)
			else
				(new alkane_combustion (alkanes @ alkanes) :> reaction)
		method is_balanced : bool = self#check_balance
	end 