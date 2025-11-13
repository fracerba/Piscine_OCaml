let ft_sum f l u =
	let rec loop l r =
		if l > u then
			nan
		else if l = u then
			r +. f l
		else
			loop (l + 1) (r +. (f l))
		in loop l 0.0

let () =
	print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10);
	print_char ('\n');

	print_float (ft_sum (fun i -> float_of_int (i * 2)) 3 9);
	print_char ('\n');

	print_float (ft_sum (fun i -> float_of_int (i)) 9 8);
	print_char ('\n')