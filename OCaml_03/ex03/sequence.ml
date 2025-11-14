let sequence n =
	let update =

	in let rec loop i n s =
		if i = n then
			s
		else
			loop (i + 1) n s
	in loop 0 n ""

let () =
	print_endline (sequence (-1));
	print_endline (sequence 1);
	print_endline (sequence 2);
	print_endline (sequence 10)