class virtual reaction (start : (Molecule.molecule * int) list) (result : (Molecule.molecule * int) list) =
	object (self)
		method virtual get_start : (Molecule.molecule * int) list
		method virtual get_result : (Molecule.molecule * int) list
		method virtual balance : reaction
		method virtual is_balanced : bool

		method private sort_start : (Molecule.molecule * int) list = 
			List.sort (fun (a, _) (b, _) -> compare (a#formula, a#name) (b#formula, b#name)) start
		method private sort_result : (Molecule.molecule * int) list =
			List.sort (fun (a, _) (b, _) -> compare (a#formula, a#name) (b#formula, b#name)) result
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
		method equals (r : reaction) =
			let rec list_cmp lst1 lst2 =
				match lst1, lst2 with
					| [], [] -> true
					| (m1, n1) :: t1, (m2, n2) :: t2 when m1#equals m2 && n1 = n2 -> list_cmp t1 t2
					| _ -> false
			in list_cmp self#sort_start r#get_start && list_cmp self#sort_result r#get_result
	end