class dalek =
	object (self)
		val name : string = 
			let rand_char () = 
				if Random.bool () then
					String.make 1 (char_of_int ((Random.int 26) + 65))
				else
					String.make 1 (char_of_int ((Random.int 26) + 97))
			in Random.self_init();
				"Dalek" ^ rand_char () ^ rand_char () ^ rand_char ()
		val hp : int = 100
		val mutable shield : bool = true

		method to_string = "name: " ^ name ^ " | hp: " ^ string_of_int hp ^ " | shield: " ^ string_of_bool shield 
		method talk = 
			let rand = 
				Random.int 4 
			in match rand with
				| 0 -> print_endline ("Explain! Explain!")
				| 1 -> print_endline ("Exterminate! Exterminate!")
				| 2 -> print_endline ("I obey!")
				| 3 -> print_endline ("You are the Doctor! You are the enemy of the Daleks!")
				| _ -> print_endline ("Error")
		method exterminate (p : People.people) =
			shield <- not shield;
			p#die;
		method die = print_endline ("Emergency Temporal Shift!")
	end