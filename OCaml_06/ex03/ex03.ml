module type FIXED = sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig val bits : int end

module type MAKE = functor (B : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE = 
	functor (B : FRACTIONNAL_BITS) ->
	struct
		type t = int

		let of_float (n : float) = int_of_float (floor (0.5 +. (n *. float_of_int (1 lsl B.bits))))

		let of_int (n : int) = n lsl B.bits

		let to_float (n : t) = float_of_int n /. float_of_int (1 lsl B.bits)

		let to_int (n : t) = n lsr B.bits

		let to_string (n : t) = string_of_float (to_float n)

		let zero = 0

		let one = of_int 1

		let succ (n : t) = n + 1

		let pred (n : t) = n - 1

		let min (n : t) (m : t) = 
			if n > m then 
				m
			else
				n

		let max (n : t) (m : t) = 
			if n < m then 
				m
			else
				n

		let gth (n : t) (m : t)  = n > m

		let lth (n : t) (m : t) = n < m

		let gte (n : t) (m : t) = n >= m

		let lte (n : t) (m : t) = n <= m

		let eqp (n : t) (m : t) = n == m (** physical equality *)

		let eqs (n : t) (m : t) = n = m (** structural equality *)

		let add (n : t) (m : t) = n + m

		let sub (n : t) (m : t) = n - m

		let mul (n : t) (m : t) = (n * m) lsr B.bits

		let div (n : t) (m : t) = (n lsl B.bits) / m

		let foreach (n : t) (m : t) f =
			let rec loop_up i =
				if i > m then
					()
				else begin
					f i; 
					loop_up (succ i)
				end
			in let rec loop_down i =
				if i < m then
					()
				else begin
					f i; 
					loop_down (pred i)
				end
			in if n < m then
				loop_up n
			else
				loop_down n
	end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
		print_endline (Fixed8.to_string r8);
		Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));

	let x4 = Fixed4.of_float 21.21 in
	let y4 = Fixed4.of_int 21 in
		print_endline "--------------------------------";
		print_endline (Fixed4.to_string x4);
		print_endline (Fixed4.to_string y4);
		print_endline (string_of_float (Fixed4.to_float x4));
		print_endline (string_of_int (Fixed4.to_int x4));
		print_newline ();

		print_endline (Fixed4.to_string Fixed4.zero);
		print_endline (Fixed4.to_string Fixed4.one);
		print_endline (Fixed4.to_string (Fixed4.succ x4));
		print_endline (Fixed4.to_string (Fixed4.pred x4));
		print_endline (Fixed4.to_string (Fixed4.min x4 y4));
		print_endline (Fixed4.to_string (Fixed4.max x4 y4));
		print_newline ();

		print_endline (string_of_bool (Fixed4.gth x4 y4));
		print_endline (string_of_bool (Fixed4.gth x4 x4));
		print_endline (string_of_bool (Fixed4.lth x4 y4));
		print_endline (string_of_bool (Fixed4.lth x4 x4));
		print_endline (string_of_bool (Fixed4.gte x4 y4));
		print_endline (string_of_bool (Fixed4.gte x4 x4));
		print_endline (string_of_bool (Fixed4.lte x4 y4));
		print_endline (string_of_bool (Fixed4.lte x4 x4));
		print_endline (string_of_bool (Fixed4.eqp x4 y4));
		print_endline (string_of_bool (Fixed4.eqp x4 x4));
		print_endline (string_of_bool (Fixed4.eqs x4 y4));
		print_endline (string_of_bool (Fixed4.eqs x4 x4));
		print_newline ();

		print_endline (Fixed4.to_string (Fixed4.add x4 y4));
		print_endline (Fixed4.to_string (Fixed4.sub x4 y4));
		print_endline (Fixed4.to_string (Fixed4.mul x4 y4));
		print_endline (Fixed4.to_string (Fixed4.div x4 y4));
		print_newline ();

		Fixed4.foreach x4 y4 (fun f -> print_endline (Fixed4.to_string f))