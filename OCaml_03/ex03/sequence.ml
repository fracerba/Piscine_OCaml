let sequence n =
	let rec loop_list l n =
		match l with
			| [] -> []
			| h :: i :: t when h = i -> loop_list (i :: t) (n + 1)
			| h :: t -> string_of_int n :: h :: loop_list t 1
	in let rec loop l n =
		if n < 0 then
			["";]
		else if n = 0 then
			l
		else
			loop (loop_list l 1) (n - 1)
	in let rec create_str l =
		match l with
			| [] -> ""
			| h :: t -> h ^ create_str t
	in create_str (loop ["1";] (n - 1))

let () =
	print_endline (sequence (-1));
	print_endline (sequence 0);
	print_endline (sequence 1);
	print_endline (sequence 2);
	print_endline (sequence 4);
	print_endline (sequence 6);
	print_endline (sequence 8)