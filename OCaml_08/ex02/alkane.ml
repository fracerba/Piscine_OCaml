class virtual alkane (nmb : int) =
	let alkane_name : string = 
		if nmb > 0 && nmb < 17 then
			List.nth ["Methane"; "Ethane"; "Propane"; "Butane"; "Pentane"; "Hexane"; "Heptane"; "Octane"; "Nonane"; 
			"Decane"; "Undecane"; "Dodecane"; "Tridecane"; "Tetradecane"; "Pentadecane"; "Hexadecane"] (nmb - 1)
		else
			"Unknown Alkane"
	in let alkane_atoms : Atom.atom list =
			(new Atom.carbon#to_list nmb) @ (new Atom.hydrogen#to_list (2 * nmb + 2))
	in object (self)
		inherit Molecule.molecule (alkane_name) (alkane_atoms)
	end

class methane =
	object
		inherit alkane 1
	end

class ethane =
	object
		inherit alkane 2
	end

class propane =
	object
		inherit alkane 3
	end

class butane =
	object
		inherit alkane 4
	end

class pentane =
	object
		inherit alkane 5
	end

class hexane =
	object
		inherit alkane 6
	end

class heptane =
	object
		inherit alkane 7
	end

class octane =
	object
		inherit alkane 8
	end

class nonane =
	object
		inherit alkane 9
	end

class decane = 
	object
		inherit alkane 10
	end

class undecane =
	object
		inherit alkane 11
	end

class dodecane =
	object
		inherit alkane 12
	end

class tridecane =
	object
		inherit alkane 13
	end

class tetradecane =
	object
		inherit alkane 14
	end

class pentadecane =
	object
		inherit alkane 15
	end

class hexadecane =
	object
		inherit alkane 16
	end