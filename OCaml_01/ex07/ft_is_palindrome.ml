let ft_is_palindrome s =
	let cmp n m = 
		String.get s n = String.get s m
	in let rec check_limits n m = 
		if n < m then begin
			if cmp n m then
				check_limits (n + 1) (m - 1)
			else
				false
		end
		else
			true
	in check_limits 0 (String.length s - 1)

let () =
	print_endline (string_of_bool (ft_is_palindrome "radar"));
	print_endline (string_of_bool (ft_is_palindrome "madam"));
	print_endline (string_of_bool (ft_is_palindrome "car"));
	print_endline (string_of_bool (ft_is_palindrome ""))