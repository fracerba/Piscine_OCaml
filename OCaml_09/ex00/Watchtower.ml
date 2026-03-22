module Watchtower =
struct
	type hour = int

	let zero : hour = 12

	let add (i : hour) (j : hour) : hour =
		if (i + j) mod zero = 0 then
			zero
		else
			(i + j) mod zero

	let sub (i : hour) (j : hour) : hour =
		if (i - j) mod zero >= 0 then begin
			if (i - j) mod zero = 0 then
				zero
			else
				(i - j) mod zero
		end
		else
			(i - j) mod zero + zero
end