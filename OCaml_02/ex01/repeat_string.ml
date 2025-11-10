let repeat_string ?(str = "x") n =
	let rec loop n a s = 
		if n < 0 then
			"Error"
		else if n = 0 then
			a
		else
			loop (n - 1) (a ^ s) s
	in loop n "" str

let () =
	print_endline (repeat_string (-1));
	print_endline (repeat_string 0);
	print_endline (repeat_string ~str:"Toto" 1);
	print_endline (repeat_string 2);
	print_endline (repeat_string ~str:"a" 5);
	print_endline (repeat_string ~str:"what" 3)