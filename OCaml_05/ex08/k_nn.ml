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

let rec print_radar_list (l : radar list) =
		match l with
			| h :: t -> print_radar h;
									print_radar_list t
			| _ -> ()

let rec find_nn (lst : radar list) : string =
	let rec extract_str l a =
		match l with
			| [] -> a
			| h :: t -> extract_str t (snd h :: a)
	in let rec update i acc =
		match acc with
			| h :: t when (fst h) = i -> (fst h, snd h + 1) :: t
			| h :: t -> List.append [h] (update i t)
			| [] -> (i, 1) :: acc
	in let rec loop l a =
		match l with
			| h :: t -> loop t (update h a)
			| _ -> a
	in let cmp_max h a =
			if snd h > snd a then
				h
			else if snd h < snd a then
				a
			else
				h (* da cambiare trovare un modo per scegliere il migliore *)
	in let rec find_max l a =
		match l with
			| h :: t -> find_max t (cmp_max h a)
			| _ -> fst a
	in find_max (loop (extract_str lst []) []) ("", 0)

let k_nn (lst : radar list) (k : int) (rdr : radar) : string =
	let cmp_radar a b =
		if eu_dist (fst a) (fst rdr) > eu_dist (fst b) (fst rdr) then
			1
		else if eu_dist (fst a) (fst rdr) < eu_dist (fst b) (fst rdr) then
			-1
		else
			0
	in let check_min a b =
		if Array.length a < k then begin
			Array.append a [|b|]
		end
		else begin
			Array.sort cmp_radar a;
			if eu_dist (fst a.(0)) (fst rdr) > eu_dist (fst b) (fst rdr) then begin
				Array.set a 0 b;
				Array.sort cmp_radar a;
				print_radar_list (Array.to_list a);
				print_newline ();
				a
			end
			else
				a
		end
	in let rec find_k_nn min lst =
		if List.length lst > 0 && Float.is_nan (eu_dist (fst (List.hd lst)) (fst rdr)) then
			"Error: invalid data"
		else
			match lst with	
				| h :: t -> find_k_nn (check_min min h) t
				| _ -> find_nn (Array.to_list min)
	in find_k_nn [||] lst

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
			print_endline (k_nn lst 1 (Array.make len 0.0, ""));
			print_endline (k_nn lst 5 (Array.make len 1.0, ""));
			(* print_endline (k_nn lst 4 (Array.make len 3.0, ""));
			print_endline (k_nn lst 7 (Array.make len (-2.0), "")); *)
		end
	end
	else
		print_endline "Error: invalid arguments"