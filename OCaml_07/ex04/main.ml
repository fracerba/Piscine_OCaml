let init_armies peoples doctors daleks =
	let list_init lst f =
		List.init (List.length lst) (fun i -> f i)
	in let peoples_fun i = new People.people (List.nth peoples i) in
	let peoples = list_init peoples peoples_fun in
	print_newline ();

	let doctors_fun i = new Doctor.doctor (List.nth doctors i) (1200 + i * 300) (List.nth peoples i) in
	let doctors = list_init doctors doctors_fun in
	print_newline ();

	let daleks = List.init 5 (fun _ -> new Dalek.dalek) in
	(peoples, doctors, daleks)

let () = 
	let peoples = ["Donna Noble"; "Amy Pond"; "Rory Williams"; "Rose Tyler"; "Clara Oswald";]
	and doctors = ["The Ninth Doctor"; "The Tenth Doctor"; "The Eleventh Doctor";]
	and dalek = 5 in

	let peoples, doctors, daleks = init_armies peoples doctors dalek in
	let galifrey = new Galifrey.galifrey peoples doctors daleks in
	galifrey#do_time_war