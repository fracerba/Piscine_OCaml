module Calc_int = Calc.Calc(Calc.INT)
module Calc_float = Calc.Calc(Calc.FLOAT)

let () =
	let prin_calc_int a b r sign =
		print_string (string_of_int a);
		if sign <> "!" then begin
			print_string (" " ^ sign ^ " ");
			print_string (string_of_int b);
		end
		else
			print_string sign;
		print_string " = ";
		print_endline (string_of_int r)
	and print_calc_float a b r sign =
		print_string (string_of_float a);
		if sign <> "!" then begin
			print_string (" " ^ sign ^ " ");
			print_string (string_of_float b);
		end
		else
			print_string sign;
		print_string " = ";
		print_endline (string_of_float r)
	in let print_calc (a : float) (b : float) (sign : string) =
		try
			match sign with
				| "+" -> print_endline (string_of_int (Calc_int.add a b))
				| "-" -> print_endline (string_of_int (Calc_int.sub a b))
				| "*" -> print_endline (string_of_int (Calc_int.mul a b))
				| "/" -> print_endline (string_of_int (Calc_int.div a b))
				| "^" -> print_endline (string_of_int (Calc_int.power a b))
				| "!" -> print_endline (string_of_int (Calc_int.fact a))
				| "+." -> print_endline (string_of_float (Calc_float.add (float_of_int a) (float_of_int b)))
				| "-." -> print_endline (string_of_float (Calc_float.sub (float_of_int a) (float_of_int b)))
				| "*." -> print_endline (string_of_float (Calc_float.mul (float_of_int a) (float_of_int b)))
				| "/." -> print_endline (string_of_float (Calc_float.div (float_of_int a) (float_of_int b)))
				| "^." -> print_endline (string_of_float (Calc_float.power (float_of_int a) b))
				| "!." -> print_endline (string_of_float (Calc_float.fact (float_of_int a)))
				| _ -> print_endline "Invalid operator"
		with Division_by_zero -> print_endline "Division by zero"
	let print_calc_int a b sign =
		print_string (string_of_int a);
		if sign <> "!" && sign <> "!." then begin
			print_string (" " ^ sign ^ " ");
			print_string (string_of_int b);
		end
		else
			print_string sign;
		print_string " = ";
		try
			match sign with
				| "+" -> print_endline (string_of_int (Calc_int.add a b))
				| "-" -> print_endline (string_of_int (Calc_int.sub a b))
				| "*" -> print_endline (string_of_int (Calc_int.mul a b))
				| "/" -> print_endline (string_of_int (Calc_int.div a b))
				| "^" -> print_endline (string_of_int (Calc_int.power a b))
				| "!" -> print_endline (string_of_int (Calc_int.fact a))
				| "+." -> print_endline (string_of_float (Calc_float.add (float_of_int a) (float_of_int b)))
				| "-." -> print_endline (string_of_float (Calc_float.sub (float_of_int a) (float_of_int b)))
				| "*." -> print_endline (string_of_float (Calc_float.mul (float_of_int a) (float_of_int b)))
				| "/." -> print_endline (string_of_float (Calc_float.div (float_of_int a) (float_of_int b)))
				| "^." -> print_endline (string_of_float (Calc_float.power (float_of_int a) b))
				| "!." -> print_endline (string_of_float (Calc_float.fact (float_of_int a)))
				| _ -> print_endline "Invalid operator"
		with Division_by_zero -> print_endline "Division by zero"
	in print_calc_int 3 5 "+";
	print_calc_float 3.0 5.0 "+.";
	print_newline ();

	print_endline (string_of_int (Calc_int.sub 3 5));
	print_endline (string_of_float (Calc_float.sub 3.0 5.0));
	print_newline ();

	print_endline (string_of_int (Calc_int.mul 3 5));
	print_endline (string_of_float (Calc_float.mul 3.0 5.0));
	print_newline ();

	print_endline (string_of_int (Calc_int.div 3 5));
	print_endline (string_of_float (Calc_float.div 3.0 5.0));
	print_endline (string_of_int (Calc_int.power 3 3));
	print_endline (string_of_float (Calc_float.power 3.0 3));
	print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
	print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))