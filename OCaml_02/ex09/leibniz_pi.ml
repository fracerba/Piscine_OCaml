let leibniz_pi d =
	let power i =
		-1.0 ** float_of_int i
	in let calc i =
		4.0 *. (power i /. float_of_int ((2 * i + 1)))
	in let diff a pi =
		if pi > a then
			pi -. a
		else
			a -. pi
	in let rec loop n a pi =
		if d < 0.0 then
			-1
		else if d > diff a pi then
			n
		else
			loop (n + 1) (a +. calc (n + 1)) pi
	in loop 0 (calc 0) (4.0 *. atan 1.0)

let () =
	print_int (leibniz_pi (-0.01));
	print_char ('\n');

	print_int (leibniz_pi 0.05);
	print_char ('\n');

	print_int (leibniz_pi 1.0);
	print_char ('\n');

	print_int (leibniz_pi 0.1);
	print_char ('\n');

	print_int (leibniz_pi 0.000001);
	print_char ('\n')