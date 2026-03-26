module Watchtower =
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
		if result >= 0 then begin
			if result = 0 then
				zero
			else
				result
		end
		else
			result + zero
end