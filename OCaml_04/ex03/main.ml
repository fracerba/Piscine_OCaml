let () =
  let deck = Deck.newDeck ()
  in let (card1, rest1) = Deck.drawCard deck
  in let (card2, rest2) = Deck.drawCard rest1
  in let rec print_deck d =
    match d with
      | [] -> ()
      | h :: t -> print_string (h ^ " ");
									print_deck t
  in print_deck (Deck.toStringList deck);
  print_endline "\n";
  print_deck (Deck.toStringListVerbose deck);
  print_endline ("\n" ^ Deck.Card.Value.toString (Deck.Card.getValue card1));
  print_endline ("\n" ^ Deck.Card.Value.toString (Deck.Card.getValue card2));
