let init_armies peoples doctors daleks =
	let list_init lst f =
		List.init (List.length lst) (fun i -> f i)
	in let peoples_fun i = new People.people (List.nth peoples i) in
	let new_peoples = list_init peoples peoples_fun in
	print_newline ();

	let doctors_fun i = new Doctor.doctor (List.nth doctors i) (1000 + i * 300) (List.nth new_peoples i) in
	let new_doctors = list_init doctors doctors_fun in
	print_newline ();

	let new_daleks = List.init daleks (fun _ -> new Dalek.dalek) in
	(new_peoples, new_doctors, new_daleks)

let () = 
	let peoples = ["Donna Noble"; "Amy Pond"; "Rory Williams"; "Rose Tyler"; "Clara Oswald";
		"Susan Foreman"; "Barbara Wright"; "Ian Chesterton"; "Steven Taylor";
		"Dodo Chaplet"; "Polly Wright"; "Ben Jackson"; "Jamie McCrimmon"; "Victoria Waterfield";]
	and doctors = ["The War Doctor"; "The Ninth Doctor"; "The Tenth Doctor"; "The Eleventh Doctor";]
	and dalek = 7 in

	let peoples, doctors, daleks = init_armies peoples doctors dalek in
	let galifrey = new Galifrey.galifrey peoples doctors daleks in
	galifrey#do_time_war