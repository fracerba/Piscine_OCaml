let encode l =
	let rec loop l n =
		match l with
			| [] -> []
			| h :: i :: t when h = i -> loop (i :: t) (n + 1)
			| h :: i :: t when h <> i -> (n, h) :: loop (i :: t) 1
			| h :: t -> (n, h) :: loop t 1
	in loop l 1

let print_tuple_first a =
	print_string "(";
	print_int a;
	print_string ", "

let print_semicolon n =
	if n = 0 then
		print_string "); "
	else
		print_string ");"

let print_bool b =
	if b then
		print_string "true"
	else
		print_string "false"

let print_tuple_bool (a, b) n =
	print_tuple_first a;
	print_bool b;
	print_semicolon n

let print_tuple_char (a, b) n =
	print_tuple_first a;
	print_char b;
	print_semicolon n

let print_tuple_int (a, b) n =
	print_tuple_first a;
	print_int b;
	print_semicolon n

let print_tuple_string (a, b) n =
	print_tuple_first a;
	print_string b;
	print_semicolon n

let print_list_bool l =
	print_char '[';
	let rec loop l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_tuple_bool h 0;
												loop (i :: t)
			| h :: t -> print_tuple_bool h 1;
									loop t
	in loop l;
	print_string "]\n"

let print_list_char l =
	print_char '[';
	let rec loop l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_tuple_char h 0;
												loop (i :: t)
			| h :: t -> print_tuple_char h 1;
									loop t;
	in loop l;
	print_string "]\n"

let print_list_int l =
	print_char '[';
	let rec loop l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_tuple_int h 0;
												loop (i :: t)
			| h :: t -> print_tuple_int h 1;
									loop t;
	in loop l;
	print_string "]\n"

let print_list_string l =
	print_char '[';
	let rec loop l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_tuple_string h 0;
												loop (i :: t)
			| h :: t -> print_tuple_string h 1;
									loop t;
	in loop l;
	print_string "]\n"

let () =
	print_list_int (encode [1; 2; 3;]);
	print_list_int (encode [1; 1; 2; 2; 2; 3; 5; 5;]);
	print_list_char (encode ['a'; 'b'; 'b'; 'a'; 'a';]);
	print_list_bool (encode [false; false; true; false; true; true; false;]);
	print_list_string (encode ["cat"; "cat"; "cat"; "dog"; "dog"; "cat";]);
	print_list_int (encode [])