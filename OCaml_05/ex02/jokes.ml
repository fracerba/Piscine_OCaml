let () =
	let jokes = [|
		"\"Mi rifiuto!\" disse il netturbino";
		"Le mie figlie hanno sposato due salumieri. Quindi ho due... generi alimentari!";
		"Due mandarini litigano furiosamente e uno dice all'altro: \"Guarda che ti spicchio!\"";
		"Ma d'inverno si leggono più libri perché hanno la copertina?";
		"Cosa fa una fabbrica di carta igienica che fallisce? Va a rotoli.";
		"Ragazzo scoppia di salute. Feriti i genitori.";
		"Qual è il colmo per due divorziati americani? Essere… stati uniti.";
	|]
	in Random.self_init();
	print_endline jokes.(Random.int (Array.length jokes))