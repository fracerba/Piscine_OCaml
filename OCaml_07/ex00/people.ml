class people (nm : string) =
	object
		val name : string = nm
		val hp : int = 100

		method to_string = name ^ " has " ^ string_of_int hp ^ " hp."
		method talk = print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")
		method die = print_endline ("Aaaarghh!")
		initializer print_endline (name ^ " was transported here by a weeping angel.")
	end