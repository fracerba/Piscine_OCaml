module Set = Set.Set

let () =
	let print_set s f =
		Set.foreach s (fun x -> print_endline (f x))
	in