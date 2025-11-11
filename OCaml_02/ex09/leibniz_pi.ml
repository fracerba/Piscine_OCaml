let leibniz_pi d =
	let calc i =
		if i mod 2 = 0 then 
			1.0 /. float_of_int ((2 * i + 1))
		else 
			-1.0 /. float_of_int ((2 * i + 1))
	in let rec loop n a =
		if d < 0 then
			-1
		else if d > (4.0 *. atan 1.0) - a || d > a - (4.0 *. atan 1.0) then
			n
		else
			loop (n + 1) (a + calc (n + 1)) 
	in loop 0 (4.0 *. calc 0)

let () =
	print_int (leibniz_pi 0.05);
	print_char ('\n');

	print_int (leibniz_pi 1.0);
	print_char ('\n');

	print_int (leibniz_pi 0.1);
	print_char ('\n')