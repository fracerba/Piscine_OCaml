let print_card c =
	print_endline ("Drawn card: " ^ (Deck.Card.toString c));
	print_endline ("Deck.Card.toString: " ^ (Deck.Card.toString c));
	print_endline ("Deck.Card.toStringVerbose: " ^ (Deck.Card.toStringVerbose c));
	print_newline ();
	print_endline ("Deck.Card.isOf(Diamond): " ^ (string_of_bool (Deck.Card.isOf c Deck.Card.Color.Diamond)));
	print_endline ("Deck.Card.isSpade: " ^ (string_of_bool (Deck.Card.isSpade c)));
	print_endline ("Deck.Card.isHeart: " ^ (string_of_bool (Deck.Card.isHeart c)));
	print_endline ("Deck.Card.isDiamond: " ^ (string_of_bool (Deck.Card.isDiamond c)));
	print_endline ("Deck.Card.isClub: " ^ (string_of_bool (Deck.Card.isClub c)));
	print_newline ();
	print_endline ("Deck.Card.Value.toString: " ^ (Deck.Card.Value.toString (Deck.Card.getValue c)));
	print_endline ("Deck.Card.Value.toInt: " ^ (string_of_int (Deck.Card.Value.toInt (Deck.Card.getValue c))));
	print_endline ("Deck.Card.Color.toString: " ^ (Deck.Card.Color.toString (Deck.Card.getColor c)));
	print_endline "\n";

let print_card_pair a b =
	print_endline ("Drawn cards: " ^ (Deck.Card.toString a) ^ " " ^ (Deck.Card.toString b));
	print_endline ("Deck.Card.compare: " ^ (string_of_int (Deck.Card.compare a b)));
	print_endline ("Deck.Card.max: " ^ (Deck.Card.toString (Deck.Card.max a b)));
	print_endline ("Deck.Card.min: " ^ (Deck.Card.toString (Deck.Card.min a b)));
	print_newline ();

let () =
	let deck = Deck.newDeck ()
	in let (card1, rest1) = Deck.drawCard deck
	in let (card2, rest2) = Deck.drawCard rest1
	in let rec print_deck d =
		match d with
			| [] -> print_newline ()
			| h :: t -> print_string (h ^ " ");
									print_deck t
	in print_deck (Deck.toStringList deck);
	print_deck (Deck.toStringListVerbose deck);
	print_card card1;
	print_card card2;
	print_card_pair card1 card2;
	print_endline "\nRemaining deck:";
	print_deck (Deck.toStringList rest2);
	print_endline ("Card.best: " ^ Deck.Card.toString (Deck.Card.best rest2))
