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
	print_endline (string_of_bool (ft_string_all is_digit "0123456789"));
	print_endline (string_of_bool (ft_string_all is_digit "O12EAS67B9"))