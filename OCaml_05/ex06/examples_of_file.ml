let examples_of_file (file : string) : (float array * string) list =
	let rec create_tuple l a : (float array * string) =
		match l with
			| h :: n :: t -> create_tuple (n :: t) (Array.append a [|float_of_string h|])
			| h :: t -> (a, h)
			| _ -> (a, "")
	in try
		let filein = 
			open_in file
		in let rec create_list l = 
			try
				create_list ((create_tuple (String.split_on_char ',' (input_line filein)) [||]) :: l)
			with End_of_file -> close_in filein;
				List.rev l
		in create_list []
	with Sys_error err -> print_endline	("Error: " ^ err);
		[]

let () =
	let print_array (a : float array) =
		print_string "([|";
		for i = 0 to (Array.length a) - 2 do begin
			print_float (Array.get a i);
			print_string "; ";
		end done;
		print_float (Array.get a ((Array.length a) - 1));
		print_string "|], ";
	in let print_example (e : (float array * string)) =
		print_array (fst e);
		print_endline ("\"" ^ (snd e) ^ "\")")
	in let rec print_examples_list (l : (float array * string) list) =
		match l with
			| h :: t -> print_example h;
									print_examples_list t
			| _ -> ()
	in if Array.length Sys.argv = 2 then
		print_examples_list (examples_of_file Sys.argv.(1))
	else
		print_endline "Error: invalid arguments"