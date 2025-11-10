let rec ft_power n e =
	if e <= 0 then
		1
	else
		n * (ft_power n (e - 1))

let () =
	print_int (ft_power 5 2);
	print_char '\n';

	print_int (ft_power 3 3);
	print_char '\n';

	print_int (ft_power 0 7);
	print_char '\n'