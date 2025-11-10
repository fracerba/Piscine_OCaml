let ft_print_rev s =
	let rec ft_print_string n =
		if n >= 0 then begin
			print_char (String.get s n);
			ft_print_string (n - 1)
		end
	in ft_print_string (String.length s - 1);
	print_char '\n'

let () =
	ft_print_rev "Hello world !";
	ft_print_rev "";
	ft_print_rev "! dlrow olleH"