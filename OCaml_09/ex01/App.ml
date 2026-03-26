module App =
struct
	type project = string * string * int

	let zero : project = ("", "", 0)

	let combine (a : project) (b : project) : project =
		let get_status g1 g2 =
			if (g1 + g2) / 2 >= 80 then
				"succeed"
			else
				"failed"
		in match a, b with
			| (n1, s1, g1), (n2, s2, g2) -> (n1 ^ n2, get_status g1 g2, (g1 + g2) / 2)
	let fail (a : project) : project =
		match a with
			| (n1, _, _) -> (n1, "failed", 0)
	let success (a : project) : project =
		match a with
			| (n1, _, _) -> (n1, "succeed", 80)
end