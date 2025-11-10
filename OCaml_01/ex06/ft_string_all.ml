let ft_string_all func s =
	let rec check_char n = 
		if n < String.length s then begin
			if func (String.get s n) then
				check_char (n + 1)
			else
				false
		end
		else
			true
	in check_char 0

let is_digit c = c >= '0' && c <= '9'

let () =
	if ft_string_all is_digit "0123456789" then
		print_endline "true"
	else
		print_endline "false";

	if ft_string_all is_digit "O12EAS67B9" then
		print_endline "true"
	else
		print_endline "false"