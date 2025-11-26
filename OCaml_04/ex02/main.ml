let () =
	let rec fib_tail_rec n a b =
		if n < 0 then
			-1
		else if n = 0 then
			a
		else if n = 1 then
			b
		else
			fib_tail_rec (n - 1) b (a + b)
	in let fibonacci n =
		fib_tail_rec n 0 1
	in let print_card c =
		print_endline ("toString: " ^ (Card.toString c));
		print_endline ("toStringVerbose: " ^ (Card.toStringVerbose c));
		print_endline ("isOf(Diamond): " ^ (string_of_bool (Card.isOf c Card.Color.Diamond)));
		print_endline ("isSpade: " ^ (string_of_bool (Card.isSpade c)));
		print_endline ("isHeart: " ^ (string_of_bool (Card.isHeart c)));
		print_endline ("isDiamond: " ^ (string_of_bool (Card.isDiamond c)));
		print_endline ("isClub: " ^ (string_of_bool (Card.isClub c)));
		print_newline ();
	in let print_card_pair a b =
		print_endline ("Cards: " ^ (Card.toString a) ^ " " ^ (Card.toString b));
		print_string "compare: ";
		print_int (Card.compare a b);
		print_endline ("\nmax: " ^ (Card.toString (Card.max a b)));
		print_endline ("min: " ^ (Card.toString (Card.min a b)));
		print_newline ();
	in let rec print_deck c =
		match c with
			| [] -> ()
			| h :: t -> print_string ((Card.toString h) ^ " ");
									print_deck t
	in let rec loop l =
		match l with
			| [] -> ()
			| h :: t -> print_card h;
									loop t
	in let rec loop_deck l =
		match l with
			| [] -> ()
			| h :: i :: t -> print_card_pair h i;
									loop_deck (i :: t)
			| h :: t -> loop_deck t
	in let cards = 
		Card.all
	in let rec deck_fib n l =
		if (fibonacci n) >= 52 then
			l
		else
			(List.nth cards (fibonacci n)) :: deck_fib (n + 1) l
	in let deck = 
		(List.nth cards 0) :: (deck_fib 2 [])
	in print_string "Cards: ";
	print_deck cards;
	print_newline ();
	print_string "Deck: ";
	print_deck deck;
	print_endline "\n";
	loop deck;
	print_newline ();
	loop_deck deck;
	print_endline ("best: " ^ Card.toString (Card.best deck))