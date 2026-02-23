class people (name : string) =
	object
		val _name = name
		val _hp = 100
		method to_string = _name ^ " has " ^ string_of_int _hp ^ " hp."
		method talk = print_endline ("I’m " ^ _name ^ "! Do you know the Doctor?")
		method die = print_endline ("Aaaarghh!")
		initializer print_endline (_name ^ " was transported here by a weeping angel.")
	end