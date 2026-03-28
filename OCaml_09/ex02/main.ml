module Calc_int = Calc.Calc(Calc.INT)
module Calc_float = Calc.Calc(Calc.FLOAT)

let () =
	let print_calc a b sign r f_str =
		let b_str = 
			if sign = "!" then 
				""
			else begin
				let lst = String.split_on_char '.' (f_str b) in
				if sign = "^" && List.length lst > 1 then
					List.nth lst 0
				else
					f_str b
			end
		and s_str =
			if sign = "!" then
				sign 
			else 
				" " ^ sign ^ " "
		in if r = None && sign <> "/" then
			print_endline "Invalid operation"
		else begin
			print_string ((f_str a) ^ s_str ^ b_str ^ " = ");
			if r <> None then
				print_endline (f_str (Option.get r))
			else
				print_endline ("Division by zero")
		end
	in let print_calc_int a b_opt sign =
		let b = 
			Option.value ~default:0 b_opt 
		in let r =
			match sign with
				| "+" -> Some (Calc_int.add a b)
				| "-" -> Some (Calc_int.sub a b)
				| "*" -> Some (Calc_int.mul a b)
				| "/" when b <> 0 -> Some (Calc_int.div a b)
				| "/" -> None
				| "^" -> Some (Calc_int.power a b)
				| "!" -> Some (Calc_int.fact a)
				| _ -> None
		in print_calc a b sign r string_of_int
	and print_calc_float a b_opt sign =
		let b =
			Option.value ~default:0.0 b_opt
		in let r =
			match sign with
				| "+" -> Some (Calc_float.add a b)
				| "-" -> Some (Calc_float.sub a b)
				| "*" -> Some (Calc_float.mul a b)
				| "/" -> Some (Calc_float.div a b)
				| "^" -> Some (Calc_float.power a (int_of_float b))
				| "!" -> Some (Calc_float.fact a)
				| _ -> None
		in print_calc a b sign r string_of_float
	in print_endline (string_of_int (Calc_int.power 3 3));
	print_endline (string_of_float (Calc_float.power 3. 3));
	print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
	print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
	print_endline "\n\n";

	print_calc_int 5 (Some 3) "+";
	print_calc_int 5 (Some (-4)) "+";
	print_calc_int 5 (Some Calc.INT.zero1) "+";
	print_newline ();

	print_calc_int 5 (Some 3) "-";
	print_calc_int 5 (Some (-4)) "-";
	print_calc_int 5 (Some Calc.INT.zero1) "-";
	print_newline ();

	print_calc_int 5 (Some 3) "*";
	print_calc_int 5 (Some (-4)) "*";
	print_calc_int 5 (Some Calc.INT.zero2) "*";
	print_newline ();

	print_calc_int 5 (Some 3) "/";
	print_calc_int 5 (Some 0) "/";
	print_calc_int 5 (Some (-4)) "/";
	print_calc_int 5 (Some Calc.INT.zero2) "/";
	print_newline ();

	print_calc_int 5 (Some 3) "^";
	print_calc_int 5 (Some (-4)) "^";
	print_calc_int 5 (Some Calc.INT.zero1) "^";
	print_calc_int 5 (Some Calc.INT.zero2) "^";
	print_newline ();

	print_calc_int 5 None "!";
	print_calc_int 0 None "!";
	print_calc_int (-5) None "!";
	print_endline "\n\n";

	print_calc_float 5.3 (Some 3.9) "+";
	print_calc_float 5.3 (Some (-4.2)) "+";
	print_calc_float 5.3 (Some Calc.FLOAT.zero1) "+";
	print_newline ();

	print_calc_float 5.3 (Some 3.9) "-";
	print_calc_float 5.3 (Some (-4.2)) "-";
	print_calc_float 5.3 (Some Calc.FLOAT.zero1) "-";
	print_newline ();

	print_calc_float 5.3 (Some 3.9) "*";
	print_calc_float 5.3 (Some (-4.2)) "*";
	print_calc_float 5.3 (Some Calc.FLOAT.zero2) "*";
	print_newline ();

	print_calc_float 5.3 (Some 3.9) "/";
	print_calc_float 5.3 (Some 0.0) "/";
	print_calc_float 5.3 (Some (-4.2)) "/";
	print_calc_float 5.3 (Some Calc.FLOAT.zero2) "/";
	print_newline ();

	print_calc_float 5.3 (Some 3.9) "^";
	print_calc_float 5.3 (Some (-4.2)) "^";
	print_calc_float 5.3 (Some Calc.FLOAT.zero1) "^";
	print_calc_float 5.3 (Some Calc.FLOAT.zero2) "^";
	print_newline ();

	print_calc_float 5.3 None "!";
	print_calc_float 0.0 None "!";
	print_calc_float (-5.3) None "!";