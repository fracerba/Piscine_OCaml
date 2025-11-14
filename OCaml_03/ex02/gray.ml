let gray n =
	let rec power n r =
		if n = 0 then
			r
		else
			power (n - 1) (r * 2)
	in let rec xor_string s n l a =
		if n = 0 then
			xor_string s (n + 1) l (String.sub s 0 1)
		else if l < n then
			a
		else if String.get s n = String.get s (n - 1) then
			xor_string s (n + 1) l (a ^ "0")
		else
			xor_string s (n + 1) l (a ^ "1")
	in let rec advance_carr s i a =
		if i < 0 then
			a
		else if String.get s i = '0' then
			String.sub s 0 i ^ "1" ^ a
		else
			advance_carr s (i - 1) ("0" ^ a)
	in let advance s l =
		if String.get s l = '1' then
			advance_carr s (l - 1) "0"
		else
			String.sub s 0 l ^ "1"
	in let rec loop s i j l =
		if j >= i then begin
			print_string (xor_string s 0 l "");
			if i < j then
				print_char ' ';
			loop (advance s l) (i + 1) j l
		end
	in let start n =
		if n > 0 then begin
			loop (String.make n '0') 0 ((power n 1) - 1) (n - 1);
			print_char '\n'
		end
	in start n

let () =
	gray 1;
	print_char '\n';

	gray 2;
	print_char '\n';

	gray 3;
	print_char '\n';

	gray 0;
	print_char '\n';

	gray 8