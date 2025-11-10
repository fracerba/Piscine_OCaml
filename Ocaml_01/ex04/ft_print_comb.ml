let ft_print_comb () =
	let rec print_nmb a b c =
		if a < 8 then begin
			if b < 9 then begin
				if c < 10 then begin
					print_int a;
					print_int b;
					print_int c;
					if a <> 7 || b <> 8 || c <> 9 then
							print_string ", ";
					print_nmb a b (c + 1)
				end
				else
					print_nmb a (b + 1) (b + 2)
			end
			else
				print_nmb (a + 1) (a + 2) (a + 3)
		end
	in print_nmb 0 1 2;
	print_string "\n"

let () =
	ft_print_comb ()