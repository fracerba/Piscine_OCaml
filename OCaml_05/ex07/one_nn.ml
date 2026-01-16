type radar = float array * string

let eu_dist (a : float array) (b : float array) : float =
	let len a =
		Array.length a
	in if len a = len b then begin
		let calc a b =
			(a -. b) ** 2.0
		in let rec sum i n acc =
			if i > n then
				acc
			else
				sum (i + 1) n (acc +. calc a.(i) b.(i))
		in if Array.length a > 0 then
			sqrt (sum 0 ((len a) - 1) 0.0)
		else
			nan
	end
	else 
		nan

let print_radar (e : radar) =
	let print_array (a : float array) =
		print_string "([|";
		for i = 0 to (Array.length a) - 2 do begin
			print_float (a.(i));
			print_string "; ";
		end done;
		print_float (a.((Array.length a) - 1));
		print_string "|], ";
	in print_array (fst e);
		print_endline ("\"" ^ (snd e) ^ "\")")

let one_nn (lst : radar list) (rdr : radar) : string =
	let check_min a b r =
		if eu_dist (fst a) (fst r) < eu_dist (fst b) (fst r) then
			a
		else
			b
	in let rec find_nn min lst =
		if List.length lst > 0 && Float.is_nan (eu_dist (fst (List.hd lst)) (fst rdr)) then
			"Error: invalid data"
		else
			match lst with	
				| h :: t -> find_nn (check_min min h rdr) t
				| _ -> snd min
	in find_nn (List.hd lst) lst

let examples_of_file (file : string) : radar list =
	let rec create_tuple l a : radar =
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
	if Array.length Sys.argv = 2 then begin
		let lst = 
			examples_of_file Sys.argv.(1)
		in let len =
			try
				Array.length (fst (List.hd lst))
			with Failure _ -> 0
		in if List.length lst > 0 && len > 0 then begin 
			print_endline (one_nn lst (Array.make len 0.0, ""));
			print_endline (one_nn lst (Array.make len 1.0, ""));
			print_endline (one_nn lst (Array.make len 3.0, ""));
			print_endline (one_nn lst (Array.make len (-2.0), ""));
		end
	end
	else
		print_endline "Error: invalid arguments"