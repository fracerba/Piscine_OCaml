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
	in let compare_molecules a b =
		compare (a#formula, a#name) (b#formula, b#name) 
	in let alkane_list : (Molecule.molecule * int) list =
		parse (List.sort compare_molecules alkanes) []
	in let carbon_nmb : int = 
		count_atoms alkanes 0 (new Atom.carbon)
	in let hydrogen_nmb : int = 
		count_atoms alkanes 0 (new Atom.hydrogen)
	in let start_list : (Molecule.molecule * int) list =
		if List.is_empty alkane_list then
			[]
		else
			alkane_list @ [(new Molecule.dioxygen, carbon_nmb + hydrogen_nmb / 4)]
	in let result_list : (Molecule.molecule * int) list =
		if List.is_empty alkane_list then
			[]
		else
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
			let rec calc_mcd m n =
				if n = 0 then
					m
				else
					calc_mcd n (m mod n)
			in let rec find_mcd lst acc =
				if acc = 1 then
					acc
				else
					match lst with
						| [] -> acc
						| h :: t -> find_mcd t (calc_mcd acc h)
			in let mcd =
				match alkane_list with
					| [] -> 1
					| (m, n) :: t -> find_mcd (List.map snd alkane_list) n
			in let rec new_alkane_list lst acc =
				match lst with
					| [] -> acc
					| (m, n) :: t -> new_alkane_list t (m#to_list n @ acc)
			in let new_alkanes nmb = 
				new_alkane_list (List.map (fun (m, n) -> (m, n / mcd * nmb)) alkane_list) []
			in if (count_atoms (new_alkanes 1) 0 (new Atom.hydrogen)) mod 4 <> 0 then
				(new alkane_combustion (new_alkanes 2) :> reaction)
			else
				(new alkane_combustion (new_alkanes 1) :> reaction)
		method is_balanced : bool = self#check_balance
		method get_incomplete_results : (int * (Molecule.molecule * int) list) list =
			let max_oxygen = 
				if self#is_balanced then
					carbon_nmb + hydrogen_nmb / 4 - 1
				else
					carbon_nmb + hydrogen_nmb / 4
			in let min_oxygen =
				if hydrogen_nmb mod 4 = 0 then
					hydrogen_nmb / 4
				else
					hydrogen_nmb / 4 + 1
			in let get_oxygen n =
				2 * n - hydrogen_nmb / 2
			in let incomplete_result_list a b c acc = 
				if a <> 2 * b + c || b + c > carbon_nmb then
					acc
				else
					List.filter (fun (_, n) -> n > 0) [
						(new Molecule.carbon_dioxide, b);
						(new Molecule.carbon_monoxide, c);
						(new Molecule.soot, carbon_nmb - b - c);
						(new Molecule.water, hydrogen_nmb / 2)
					] :: acc
			in let rec get_list a b c acc =
				if b = 0 then
					incomplete_result_list a b c acc
				else
					get_list a (b - 1) (c + 2) (incomplete_result_list a b c acc)
			in let sort_incomplete_result lst =
				List.sort (fun (a, _) (b, _) -> compare_molecules a b) lst
			in let get_incomplete_result n m =
				List.map (fun a -> (m, sort_incomplete_result a)) (get_list n (n / 2) (n mod 2) [])
			in let rec get_incomplete_results_list acc n =
				if n > max_oxygen then
					List.sort (fun (a, _) (b, _) -> compare a b) acc
				else
					get_incomplete_results_list ((get_incomplete_result (get_oxygen n) n) @ acc) (n + 1)
			in get_incomplete_results_list [] min_oxygen
	end