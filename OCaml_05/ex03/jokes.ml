let () =
  let jokes = 
	in Random.self_init();
	print_endline jokes.(Random.int (Array.length jokes))