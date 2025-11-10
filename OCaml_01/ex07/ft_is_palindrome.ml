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
	if ft_is_palindrome "radar" then 
		print_endline "true"
	else
		print_endline "false";

	if ft_is_palindrome "madam" then 
		print_endline "true"
	else
		print_endline "false";

	if ft_is_palindrome "car" then 
		print_endline "true"
	else
		print_endline "false";

	if ft_is_palindrome "" then 
		print_endline "true"
	else
		print_endline "false"