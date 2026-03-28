module type MONOID =
sig
	type element
	val zero1 : element
	val zero2 : element
	val mul : element -> element -> element
	val add : element -> element -> element
	val div : element -> element -> element
	val sub : element -> element -> element
end

module INT =
struct
	type element = int
	let zero1 = 0
	let zero2 = 1
	let mul (a : element) (b : element) : element = a * b
	let add (a : element) (b : element) : element = a + b
	let div (a : element) (b : element) : element = a / b
	let sub (a : element) (b : element) : element = a - b
end

module FLOAT =
struct
	type element = float
	let zero1 = 0.0
	let zero2 = 1.0
	let mul (a : element) (b : element) : element = a *. b
	let add (a : element) (b : element) : element = a +. b
	let div (a : element) (b : element) : element = a /. b
	let sub (a : element) (b : element) : element = a -. b
end

module type CALC =
	functor (M : MONOID) ->
	sig
		val add : M.element -> M.element -> M.element
		val sub : M.element -> M.element -> M.element
		val mul : M.element -> M.element -> M.element
		val div : M.element -> M.element -> M.element
		val power : M.element -> int -> M.element
		val fact : M.element -> M.element
	end

module Calc : CALC =
	functor (M : MONOID) ->
	struct
		let add (a : M.element) (b : M.element) : M.element = M.add a b
		let sub (a : M.element) (b : M.element) : M.element = M.sub a b
		let mul (a : M.element) (b : M.element) : M.element = M.mul a b
		let div (a : M.element) (b : M.element) : M.element = M.div a b
		let power (a : M.element) (n : int) : M.element = 
			let rec loop n acc =
				if n < 0 then
					M.zero1
				else if n = 0 then
					M.mul acc M.zero2
				else
					loop (n - 1) (M.mul acc a)
			in loop n M.zero2
		let fact (a : M.element) : M.element =
			let rec loop n acc =
				if n < M.zero1 then
					M.zero1
				else if n = M.zero1 || n < M.zero2 then
					acc
				else
					loop (M.sub n M.zero2) (M.mul acc n)
			in loop a M.zero2
	end