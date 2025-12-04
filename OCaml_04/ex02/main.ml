let fibonacci n =
	let rec fib_tail_rec n a b =
		if n < 0 then
			-1
		else if n = 0 then
			a
		else if n = 1 then
			b
		else
			fib_tail_rec (n - 1) b (a + b)
	in fib_tail_rec n 0 1

let print_card c =
	print_endline ("Card: " ^ (Card.toString c));
	print_endline ("Card.toString: " ^ (Card.toString c));
	print_endline ("Card.toStringVerbose: " ^ (Card.toStringVerbose c));
	print_newline ();
	print_endline ("Card.isOf(Diamond): " ^ (string_of_bool (Card.isOf c Card.Color.Diamond)));
	print_endline ("Card.isSpade: " ^ (string_of_bool (Card.isSpade c)));
	print_endline ("Card.isHeart: " ^ (string_of_bool (Card.isHeart c)));
	print_endline ("Card.isDiamond: " ^ (string_of_bool (Card.isDiamond c)));
	print_endline ("Card.isClub: " ^ (string_of_bool (Card.isClub c)));
	print_newline ();
	print_endline ("Card.getValue: " ^ (Card.Value.toString (Card.getValue c)));
	print_endline ("Card.getColor: " ^ (Card.Color.toString (Card.getColor c)));
	print_endline "\n"

let print_card_pair a b =
	print_endline ("Cards: " ^ (Card.toString a) ^ " " ^ (Card.toString b));
	print_endline ("Card.compare: " ^ (string_of_int (Card.compare a b)));
	print_endline ("Card.max: " ^ (Card.toString (Card.max a b)));
	print_endline ("Card.min: " ^ (Card.toString (Card.min a b)));
	print_newline ()

let cards = Card.all

let deck =
	let rec deck_fib n l =
		if (fibonacci n) >= 52 then
			l
		else
			(List.nth cards (fibonacci n)) :: deck_fib (n + 1) l
	in (List.nth cards 0) :: (deck_fib 2 [])

let () =
	let rec print_deck c =
		match c with
			| [] -> print_newline ()
			| h :: t -> print_string ((Card.toString h) ^ " ");
									print_deck t
	in let rec card_loop l =
		match l with
			| [] -> print_newline ()
			| h :: t -> print_card h;
									card_loop t
	in let rec cards_pair_loop l =
		match l with
			| [] -> print_newline ()
			| h :: i :: t -> print_card_pair h i;
									cards_pair_loop (i :: t)
			| h :: t -> cards_pair_loop t
	in print_string "Cards: ";
	print_deck cards;
	print_string "Deck: ";
	print_deck deck;
	print_newline ();
	print_endline "Cards Info:";
	card_loop deck;
	print_endline "Card Pairs:";
	cards_pair_loop deck;
	print_endline ("Card.best: " ^ Card.toString (Card.best deck))