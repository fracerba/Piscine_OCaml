type phosphate = string

type deoxyribose = string

type nucleobase =
	| A
	| T
	| C
	| G
	| U
	| None

type nucleotide = {
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase;
}

type helix = nucleotide list

type rna = nucleobase list

let generate_nucleotide b : nucleotide = {
	phosphate = "phosphate";
	deoxyribose = "deoxyribose";
	nucleobase = match b with
		| 'A' -> A 
		| 'T' -> T 
		| 'C' -> C 
		| 'G' -> G 
		| _ -> None
}

let rec generate_helix n : helix =
	let random_gen () =
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
	in Random.self_init ();
	loop n []

let rec helix_to_string (hlx : helix) =
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

let rec complementary_helix (hlx : helix) : helix =
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

let rec generate_rna (hlx : helix) : rna =
	let complementary_rna n =
		match n.nucleobase with
			| A -> U
			| T -> A
			| C -> G
			| G -> C
			| _ -> None
	in match hlx with
		| [] -> []
		| h :: t -> complementary_rna h :: generate_rna t

let () =
	let get_nucleobase b =
		match b with
			| A -> "A"
			| U -> "U"
			| C -> "C"
			| G -> "G"
			| _ -> "N"
	in let rec rna_to_string (l : rna) =
		match l with
			| [] -> ""
			| h :: t -> get_nucleobase h ^ rna_to_string t
	in let a = generate_helix 5
	in let b = generate_helix 7
	in print_endline ("DNA: " ^ (helix_to_string a));
	print_endline ("     " ^ (helix_to_string (complementary_helix a)));
	print_endline ("RNA: " ^ (rna_to_string (generate_rna a)));
	print_char '\n';

	print_endline ("DNA: " ^ (helix_to_string b));
	print_endline ("     " ^ (helix_to_string (complementary_helix b)));
	print_endline ("RNA: " ^ (rna_to_string (generate_rna b)));