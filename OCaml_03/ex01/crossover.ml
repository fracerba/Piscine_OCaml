let crossover l m =
	let rec search m i =
		match m with
			| [] -> false
			| h :: t when h = i -> true
			| _ :: t -> search t i
	in let rec loop l a =
		match l with
			| [] -> []
			| h :: t when (search m h) && not (search a h) -> h :: loop t (h :: a)
			| _ :: t -> loop t a 
	in loop l []

let print_semicolon n =
	if n = 0 then
		print_string "; "

let print_value_bool b n =
	print_string (string_of_bool b);
	print_semicolon n

let print_value_char b n =
	print_char b;
	print_semicolon n

let print_value_int b n =
	print_int b;
	print_semicolon n

let print_value_string b n =
	print_string b;
	print_semicolon n

let print_list_bool l =
	print_char '[';
	let rec loop l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_value_bool h 0;
												loop (i :: t)
			| h :: t -> print_value_bool h 1;
									loop t
	in loop l;
	print_string "]\n"

let print_list_char l =
	print_char '[';
	let rec loop l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_value_char h 0;
												loop (i :: t)
			| h :: t -> print_value_char h 1;
									loop t;
	in loop l;
	print_string "]\n"

let print_list_int l =
	print_char '[';
	let rec loop l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_value_int h 0;
												loop (i :: t)
			| h :: t -> print_value_int h 1;
									loop t;
	in loop l;
	print_string "]\n"

let print_list_string l =
	print_char '[';
	let rec loop l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_value_string h 0;
												loop (i :: t)
			| h :: t -> print_value_string h 1;
									loop t;
	in loop l;
	print_string "]\n"

let () =
	print_list_int (crossover [1; 2; 3] [1; 4; 5; 3]);
	print_list_int (crossover [1; 1; 2; 2; 3; 5; 5] [2; 5; 3]);
	print_list_char (crossover ['a'; 'b'; 'c'; 'd'; 'e'] ['a'; 's'; 'd']);
	print_list_bool (crossover [false; false; true; false; true; true; false] [false]);
	print_list_string (crossover ["cat"; "dog"] ["cat"; "car"; "card"; "dog"; "dig"; "data"]);
	print_list_int (crossover [0; 3] [1; 2]);
	print_list_int (crossover [] [1; 2])