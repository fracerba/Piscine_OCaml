let () = 
let people = new People.people "Clara Oswald" in
  let doctor = new Doctor.doctor "The Doctor" 2000 people in
	print_endline doctor#to_string;
	print_endline people#to_string;
	doctor#talk;
	people#talk;
  doctor#travel_in_time 2000 (-200);
  doctor#use_sonic_screwdriver;
  doctor#die;
  print_endline doctor#to_string;
	people#die;
