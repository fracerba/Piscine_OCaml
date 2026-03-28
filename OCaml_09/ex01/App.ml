module type App =
sig
	type project = string * string * int
	val zero : project
	val combine : project -> project -> project
	val fail : project -> project
	val success : project -> project
end


module App : App =
struct
	type project = string * string * int

	let zero : project = ("", "", 0)

	let combine ((n1, s1, g1) : project) ((n2, s2, g2) : project) : project =
		let get_status g1 g2 =
			if (g1 + g2) / 2 >= 80 then
				"succeed"
			else
				"failed"
		in (n1 ^ n2, get_status g1 g2, (g1 + g2) / 2)
	let fail ((n1, _, _) : project) : project =
		(n1, "failed", 0)
	let success ((n1, _, _) : project) : project =
		(n1, "succeed", 80)
end