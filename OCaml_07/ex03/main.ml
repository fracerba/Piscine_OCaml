let () = 
	let make_list n f = 
		let rec aux acc n =
			if n = 0 then 
				acc
			else 
				aux (f () :: acc) (n - 1)
		in aux [] n
	in let army = new Army.army (make_list 5 (fun () -> new Dalek.dalek)) in
  let people = new Army.army (make_list  3 (fun () -> new People.people "Clara Oswald")) in
  let doctor = new Doctor.doctor "The Doctor" 2000 people in
	print_endline people#to_string;
	print_endline doctor#to_string;
  print_endline dalek#to_string;
	print_newline ();

	people#talk;
	doctor#talk;
	print_newline ();

  let doctor2 = doctor#travel_in_time 2012 12000 in
  print_endline doctor2#to_string;
	print_newline ();

	let people2 = new People.people "Jo Patterson" in 
	let people3 = new People.people "Leo Rugazzi" in 
	print_endline people2#to_string;
	print_endline people3#to_string;
	people2#talk;
	people3#talk;
	print_newline ();

  dalek#talk;
	dalek#talk;
	dalek#talk;
	print_newline ();

  let doctor3 = doctor2#take_damage 80 in
  print_endline doctor3#to_string;
	let doctor4 = doctor3#take_damage 30 in
  print_endline doctor4#to_string;
	print_newline ();

  dalek#exterminate people2;
	print_endline dalek#to_string;
	dalek#exterminate people3;
	print_endline dalek#to_string;
	print_newline ();

  doctor#use_sonic_screwdriver;
  dalek#die;