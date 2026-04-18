module Calc_int = Calc.Calc(Calc.INT)
module Calc_float = Calc.Calc(Calc.FLOAT)

let () =
	let calc a b sign r f_str =
		let b_str = 
			if sign = "!" then 
				""
			else
				let lst = String.split_on_char '.' (f_str b) in
				let str_of_b =
					if sign = "^" && List.length lst > 1 then
						List.nth lst 0
					else
						f_str b
				in match str_of_b.[0] with
					| '-' -> "(" ^ str_of_b ^ ")"
					| _ -> str_of_b
		and s_str =
			match sign with
				| "!" -> sign
				| _ -> " " ^ sign ^ " "
		in if r = None && sign <> "/" then
			"Invalid operation"
		else
			let str = (f_str a) ^ s_str ^ b_str ^ " = " in
			match r with
				| Some r -> str ^ (f_str r)
				| None -> str ^ "Division by zero"
	in let calc_int a b_opt sign =
		let b = 
			Option.value ~default:0 b_opt 
		in let r =
			try
				match sign with
					| "+" -> Some (Calc_int.add a b)
					| "-" -> Some (Calc_int.sub a b)
					| "*" -> Some (Calc_int.mul a b)
					| "/" -> Some (Calc_int.div a b)
					| "^" -> Some (Calc_int.power a b)
					| "!" -> Some (Calc_int.fact a)
					| _ -> None
			with Division_by_zero -> None
		in calc a b sign r string_of_int
	and calc_float a b_opt sign =
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
		in calc a b sign r string_of_float
	and print_calc calc_f a b c d e sign =
		print_endline (calc_f a (Some b) sign);
		print_endline (calc_f a (Some c) sign);
		print_endline (calc_f a (Some d) sign);
		print_endline (calc_f a (Some e) sign);
		print_newline ();
	in print_endline (string_of_int (Calc_int.power 3 3));
	print_endline (string_of_float (Calc_float.power 3. 3));
	print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
	print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
	print_endline "\n";

	let i0 : Calc_int.t = Calc.INT.zero1
	and i02 : Calc_int.t = Calc.INT.zero2
	and i1 : Calc_int.t = 5
	and i2 : Calc_int.t = 3
	and i3 : Calc_int.t = -4
	and signs = ["+"; "-"; "*"; "/"; "^";] in

	print_endline "Module Calc_int:";
	List.iter (fun sgn -> print_calc calc_int i1 i2 i3 i0 i02 sgn) signs;

	print_endline (calc_int i1 None "!");
	print_endline (calc_int i0 None "!");
	print_endline (calc_int i3 None "!");
	print_endline "\n";

	let f0 : Calc_float.t = Calc.FLOAT.zero1
	and f02 : Calc_float.t = Calc.FLOAT.zero2
	and f1 : Calc_float.t = 5.3
	and f2 : Calc_float.t = 3.9
	and f3 : Calc_float.t = -4.2 in

	print_endline "Module Calc_float:";
	List.iter (fun sgn -> print_calc calc_float f1 f2 f3 f0 f02 sgn) signs;

	print_endline (calc_float f1 None "!");
	print_endline (calc_float f0 None "!");
	print_endline (calc_float f3 None "!");