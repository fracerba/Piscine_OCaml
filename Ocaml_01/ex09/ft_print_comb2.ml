let ft_print_comb2 () =
	let rec print_nmb2 a b c d =
		if a < 10 then begin
			if b < 10 then begin
				if c < 10 then begin
					if d < 10 then begin
						print_int a;
						print_int b;
						print_char ' ';
						print_int c;
						print_int d;
						if a <> 9 || b <> 8 || c <> 9 || d <> 9 then begin
							print_char ',';
							print_char ' '
						end;
						print_nmb2 a b c (d + 1)
					end
					else
						print_nmb2 a b (c + 1) 0
				end
				else
					print_nmb2 a (b + 1) a (b + 2)
			end
			else
				print_nmb2 (a + 1) 0 (a + 1) 1
		end
	in print_nmb2 0 0 0 1;
	print_string "\n"

let () =
	ft_print_comb2 ()