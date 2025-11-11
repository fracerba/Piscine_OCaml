let fibonacci n =
	let rec fib_tail_rec n a b =
		if n < 0 then
			-1
		else if n = 0 then
			a
		else if n = 1 then
			b
		else
			fib_tail_rec (n - 1) b (a + b)
	in fib_tail_rec n 0 1

let () =
	print_int (fibonacci (-42));
	print_char ('\n');

	print_int (fibonacci 1);
	print_char ('\n');

	print_int (fibonacci 3);
	print_char ('\n');

	print_int (fibonacci 6);
	print_char ('\n')