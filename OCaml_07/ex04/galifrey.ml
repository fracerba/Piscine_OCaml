class galifrey =
	object (self)
		val daleks : Dalek.dalek list = []
		val doctors : Doctor.doctor list = []
		val people : People.people list = []

		method do_time_war = 
			print_endline "The Time War has begun!";
			
		method private select_attack (attacker : 'a) (defender : 'b) = 
			
		method private check_alive (entities : 'a list) =
			
end