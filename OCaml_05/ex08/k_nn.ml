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

let rec print_radar_list (l : radar list) (rdr : radar) =
	let print_radar_aux h =
		print_string "\t";
		print_float (eu_dist_radar h rdr);
		print_string " - ";
		print_radar h;
	in match l with
		| h :: t -> print_radar_aux h;
								print_radar_list t rdr
		| _ -> ()

let rec print_max l =
	let print_aux (a, b, c) =
		print_string ("(\"" ^ a ^ "\", ");
		print_int b;
		print_string ", ";
		print_float c;
		print_string ")\n";
	in match l with
		| h :: t -> print_aux h;
								print_max t
		| _ -> ()

let rec find_nn (lst : radar list) (rdr : radar): string =
	let rec extract_str l a =
		match l with
			| h :: t -> extract_str t ((snd h, eu_dist_radar h rdr) :: a)
			| [] -> a
	in let rec update (s, f) acc =
		match acc with
			| (a, b, c) :: t when a = s -> (a, b + 1, c +. f) :: t
			| h :: t -> h :: (update (s, f) t)
			| [] -> (s, 1, f) :: acc
	in let rec loop l a =
		match l with
			| h :: t -> loop t (update h a)
			| _ -> a
	in let cmp_max (a1, b1, c1) (a2, b2, c2) =
		if b1 > b2 then
			(a1, b1, c1)
		else if b2 > b1 then
			(a2, b2, c2)
		else begin
			if c1 < c2 then
				(a1, b1, c1)
			else
				(a2, b2, c2)
		end 
	in let rec find_max l (a, b, c) =
		match l with
			| h :: t -> find_max t (cmp_max h (a, b, c))
			| _ -> a
	in (* print_max (loop (extract_str lst []) []); *)
		find_max (loop (extract_str lst []) []) ("", 0, 0.0)

let k_nn (lst : radar list) (k : int) (rdr : radar) : string =
	let cmp_radar a b =
		if eu_dist_radar a rdr > eu_dist_radar b rdr then
			-1
		else if eu_dist_radar a rdr < eu_dist_radar b rdr then
			1
		else
			0
	in let check_min a b =
		if Array.length a > 1 then
			Array.sort cmp_radar a;
		if Array.length a < k then begin
			Array.append a [|b|]
		end
		else begin
			(* print_radar_dist a.(0) b rdr; *)
			if eu_dist_radar a.(0) rdr > eu_dist_radar b rdr then begin
				Array.set a 0 b;
				if Array.length a > 1 then
					Array.sort cmp_radar a;
				(* print_radar_list (Array.to_list a) rdr; *)
				a
			end
			else
				a
		end
	in let rec find_k_nn min lst =
		if List.length lst > 0 && Float.is_nan (eu_dist_radar (List.hd lst) rdr) then
			"Error: invalid data"
		else
			match lst with	
				| h :: t -> find_k_nn (check_min min h) t
				| _ -> find_nn (Array.to_list min) rdr
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
			print_endline (k_nn lst 1 (Array.make len 0.1, ""));
			print_endline (k_nn lst 5 (Array.make len 0.3, ""));
			print_endline (k_nn lst 4 (Array.make len 0.8, ""));
			print_endline (k_nn lst 7 (Array.make len (-0.2), ""));
			print_endline (k_nn lst 20 (Array.make len (-0.9), ""));
		end
	end
	else
		print_endline "Error: invalid arguments"