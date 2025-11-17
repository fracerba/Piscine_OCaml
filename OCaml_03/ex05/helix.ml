type phosphate = string

type deoxyribose = string

type nucleobase =
	| A 
	| T 
	| C 
	| G 
	| None

type nucleotide = {
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase;
}

type helix = nucleotide list

let generate_nucleotide b = {
	phosphate = "phosphate";
	deoxyribose = "deoxyribose";
	nucleobase = match b with
		| 'A' -> A 
		| 'T' -> T 
		| 'C' -> C 
		| 'G' -> G 
		| _ -> None
}

let rec generate_helix n =
	let random_gen () =
		Random.self_init ();
		match Random.int 4 with
			| 0 -> generate_nucleotide 'A'
			| 1 -> generate_nucleotide 'T'
			| 2 -> generate_nucleotide 'C'
			| 3 -> generate_nucleotide 'G'
			| _ -> generate_nucleotide 'N'
	in let rec loop n l =
		if n <= 0 then
			l
		else
			loop (n - 1) (random_gen () :: l)
	in loop n []

let rec helix_to_string hlx =
	let get_nucleobase n =
		match n.nucleobase with
			| A -> "A"
			| T -> "T"
			| C -> "C"
			| G -> "G"
			| _ -> "N"
	in match hlx with
		| [] -> ""
		| h :: t -> get_nucleobase h ^ helix_to_string t

let rec complementary_helix hlx =
	let complementary_nucleobase n =
		match n.nucleobase with
			| A -> generate_nucleotide 'T'
			| T -> generate_nucleotide 'A'
			| C -> generate_nucleotide 'G'
			| G -> generate_nucleotide 'C'
			| _ -> generate_nucleotide 'N'
	in match hlx with
		| [] -> []
		| h :: t -> complementary_nucleobase h :: complementary_helix t

let () =
	let a = generate_helix 4
	in let b = generate_helix 7
	in print_endline (helix_to_string (generate_helix 5));
	print_endline (helix_to_string a);
	print_endline (helix_to_string (complementary_helix a));
	print_endline (helix_to_string b);
	print_endline (helix_to_string (complementary_helix b))
