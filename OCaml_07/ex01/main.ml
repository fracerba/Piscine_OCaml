let () = 
	let people = new People.people "Clara Oswald" in
	let doctor = new Doctor.doctor "The Doctor" 2000 people in
	print_endline people#to_string;
	print_endline doctor#to_string;
	people#talk;
	doctor#talk;
	doctor#use_sonic_screwdriver;
	let doctor2 = doctor#travel_in_time 2000 (-200) in
		print_endline doctor2#to_string;
	let doctor3 = doctor2#take_damage 80 in
		print_endline doctor3#to_string;
	let doctor4 = doctor3#take_damage 30 in
		print_endline doctor4#to_string;
		people#die;
