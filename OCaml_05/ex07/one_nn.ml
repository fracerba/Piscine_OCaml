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

let eu_dist_radar (a : radar) (b : radar) : float =
	eu_dist (fst a) (fst b)
		
let print_radar (e : radar) =
	let print_array (a : float array) =
		print_string "([|";
		for i = 0 to (Array.length a) - 1 do
			print_float (a.(i));
			if i < (Array.length a) - 1 then
				print_string "; ";
		done;
		print_string "|], ";
	in print_array (fst e);
		print_endline ("\"" ^ (snd e) ^ "\")")

let print_radar_dist (a : radar) (b : radar) (r : radar) =
	if eu_dist_radar a r <= eu_dist_radar b r then begin
		print_float (eu_dist_radar b r);
		print_newline ()
	end
	else begin
		print_float (eu_dist_radar b r);
		print_string " < ";
		print_float (eu_dist_radar a r);
		print_string  " --> ";
		print_radar b;
	end

let one_nn (lst : radar list) (rdr : radar) : string =
	let check_min a b r =
		(* print_radar_dist a b r; *)
		if eu_dist_radar a r <= eu_dist_radar b r then
			a
		else
			b
	in let rec find_nn min lst =
		if List.length lst > 0 && Float.is_nan (eu_dist_radar (List.hd lst) rdr) then
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
			print_endline (one_nn lst (Array.make len 0.1, ""));
			print_endline (one_nn lst (Array.make len 0.3, ""));
			print_endline (one_nn lst (Array.make len 0.8, ""));
			print_endline (one_nn lst (Array.make len (-0.2), ""));
		end
	end
	else
		print_endline "Error: invalid arguments"