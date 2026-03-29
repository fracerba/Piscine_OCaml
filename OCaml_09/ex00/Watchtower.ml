module type WATCHTOWER =
sig
	type hour = int
	val zero : hour
	val add : hour -> hour -> hour
	val sub : hour -> hour -> hour
end

module Watchtower : WATCHTOWER =
struct
	type hour = int

	let zero : hour = 12

	let add (i : hour) (j : hour) : hour =
		let result = (i + j) mod zero in
		if result = 0 then
			zero
		else
			result

	let sub (i : hour) (j : hour) : hour =
		let result = (i - j) mod zero in
		if result = 0 then
			zero
		else if result > 0 then
			result
		else
			result + zero
end