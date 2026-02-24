class doctor (nm : string) (old : int) (sdk : People.people) =
	object (self)
		val name : string = nm
		val age : int = old
		val sidekick : People.people = sdk
		val hp : int = 100
		
		method to_string = name ^ " is " ^ string_of_int age ^ " years old and has " ^ string_of_int hp ^ " hp, and his sidekick is " ^ sdk#to_string
		method talk = print_endline ("Hi! I'm the Doctor!")
		initializer print_endline (name ^ " exited the TARDIS and is ready to explore the world with " ^ sdk#to_string ^ "!")
		method travel_in_time (start : int) (arrival : int) =
			print_endline ("Traveling in time from " ^ string_of_int start ^ " to " ^ string_of_int arrival);
			print_endline ("                 _.--._");
			print_endline ("                 _|__|_");
			print_endline ("     _____________|__|_____________");
			print_endline ("  .-'______________________________'-.");
			print_endline ("  | |________POLICE___BOX__________| |");
			print_endline ("  |  |============================|  |");
			print_endline ("  |  | .-----------..-----------. |  |");
			print_endline ("  |  | |  _  _  _  ||  _  _  _  | |  |");
			print_endline ("  |  | | | || || | || | || || | | |  |");
			print_endline ("  |  | | |_||_||_| || |_||_||_| | |  |");
			print_endline ("  |  | | | || || | || | || || | | |  |");
			print_endline ("  |  | | |_||_||_| || |_||_||_| | |  |");
			print_endline ("  |  | |  _______  ||  _______  | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |_______| || |_______| | |  |");
			print_endline ("  |  | |  _______ @||@ _______  | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |_______| || |_______| | |  |");
			print_endline ("  |  | |  _______  ||  _______  | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |       | || |       | | |  |");
			print_endline ("  |  | | |_______| || |_______| | |  |");
			print_endline ("  |  | '-----------''-----------' |  |");
			print_endline (" _|__|/__________________________\\|__|_");
			print_endline ("'----'----------------------------'----'")
		method use_sonic_screwdriver = print_endline ("Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii")
		method private regenerate = new doctor name age sidekick
		method die = 
			print_endline ("The Doctor regenerated!");
			self#regenerate;
	end