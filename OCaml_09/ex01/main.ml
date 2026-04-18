let () =
	let print_proj (a : App.App.project) : unit =
		let (n, s, g) = a in
		print_string ("Name: \"" ^ n ^ "\", ");
		print_string ("Status: " ^ s ^ ", ");
		print_endline ("Grade: " ^ string_of_int g)
	in let print_proj_lst lst =
		List.iter print_proj lst;
		print_newline ()
	in let proj1 : App.App.project = ("Astro Bot", "succeed" , 94)
	and proj2 : App.App.project = ("Bloodborne", "succeed" , 92)
	and proj3 : App.App.project = ("Concord", "failed", 62)
	and proj4 : App.App.project = App.App.zero in
	let proj_lst = [proj1; proj2; proj3; proj4] in

	print_endline "Projects:";
	print_proj_lst proj_lst;

	print_endline "Combined projects:";
	let rec comb_proj lst acc = 
		match lst with
			| h :: n :: t -> comb_proj (n :: t) (App.App.combine h n :: acc)
			| _ -> List.rev acc
	in print_proj_lst (comb_proj proj_lst []);

	print_endline "Failed projects:";
	print_proj_lst (List.map App.App.fail proj_lst);

	print_endline "Successful projects:";
	print_proj_lst (List.map App.App.success proj_lst)