let ft_print_alphabet () =
	let rec ft_print_char c =
		if c < 123 then begin
			print_char (char_of_int c);
			ft_print_char (c + 1)
		end
	in ft_print_char 97;
	print_char '\n'

let () =
	ft_print_alphabet ()