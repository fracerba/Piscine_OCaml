let print_proj (a : App.App.project) : unit =
	let (n, s, g) = a in
	print_string ("Name: " ^ n ^ ", ");
	print_string ("Status: " ^ s ^ ", ");
	print_endline ("Grade: " ^ string_of_int g)

let () =
	let proj1 : App.App.project = ("Astro Bot", "succeed" , 94)
	and proj2 : App.App.project = ("Bloodborne", "succeed" , 92)
	and proj3 : App.App.project = ("Concord", "failed", 62)
	and proj4 : App.App.project = App.App.zero in
	print_proj proj1;
	print_proj proj2;
	print_proj proj3;
	print_proj proj4;
	print_newline ();

	let proj_comb1 = App.App.combine proj1 proj2
	and proj_comb2 = App.App.combine proj2 proj3
	and proj_comb4 = App.App.combine proj3 proj4 in
	print_proj proj_comb1;
	print_proj proj_comb2;
	print_proj proj_comb4;
	print_newline ();

	let proj1_fail : App.App.project = App.App.fail proj1
	and proj2_fail : App.App.project = App.App.fail proj2
	and proj3_fail : App.App.project = App.App.fail proj3 
	and proj4_fail : App.App.project = App.App.fail proj4 in
	print_proj proj1_fail;
	print_proj proj2_fail;
	print_proj proj3_fail;
	print_proj proj4_fail;
	print_newline ();
	
	let proj1_success : App.App.project = App.App.success proj1
	and proj2_success : App.App.project = App.App.success proj2
	and proj3_success : App.App.project = App.App.success proj3
	and proj4_success : App.App.project = App.App.success proj4 in
	print_proj proj1_success;
	print_proj proj2_success;
	print_proj proj3_success;
	print_proj proj4_success;