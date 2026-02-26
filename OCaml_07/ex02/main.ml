let () = 
	Random.self_init();
	let people = new People.people "Donna Noble" in
	let doctor = new Doctor.doctor "The Doctor" 2000 people in
	print_newline ();

	print_endline people#to_string;
	print_endline doctor#to_string;
	print_newline ();

	people#talk;
	doctor#talk;
	print_newline ();

	let doctor2 = doctor#travel_in_time 4012 2009 in
	print_endline doctor2#to_string;
	print_newline ();

	let dalek = new Dalek.dalek in
	let people2 = new People.people "Suzanne" in 
	let people3 = new People.people "General Sanchez" in
	let people4 = new People.people "Laura" in 
	print_newline ();

	print_endline people2#to_string;
	print_endline people3#to_string;
	print_endline people4#to_string;
	print_endline dalek#to_string;
	print_newline ();

	people2#talk;
	people3#talk;
	people4#talk;
	print_newline ();

	dalek#talk;
	dalek#talk;
	dalek#talk;
	print_newline ();

	let doctor3 = doctor2#take_damage 80 in
	print_endline doctor3#to_string;
	print_newline ();

	dalek#exterminate people2;
	print_endline dalek#to_string;
	dalek#exterminate people3;
	print_endline dalek#to_string;
	print_newline ();

	let doctor4 = doctor3#take_damage 30 in
	print_endline doctor4#to_string;
	print_newline ();

	dalek#exterminate people4;
	print_endline dalek#to_string;
	print_newline ();

	doctor#use_sonic_screwdriver;
	dalek#die;