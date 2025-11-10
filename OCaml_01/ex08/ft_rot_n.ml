let ft_rot_n n s =
	let shift_char c n m =
		char_of_int((((int_of_char c) - m + n) mod 26) + m)
	in let shift c = 
		if c >= 'A' && c <= 'Z' then 
			shift_char c n 65
		else if c >= 'a' && c <= 'z' then
			shift_char c n 97
		else
			c
	in String.map shift s

let () =
	print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
	print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
	print_endline (ft_rot_n 42 "0123456789");
	print_endline (ft_rot_n 2 "OI2EAS67B9");
	print_endline (ft_rot_n 0 "Damned !");
	print_endline (ft_rot_n 42 "");
	print_endline (ft_rot_n 1 "NBzlk qnbjr !")