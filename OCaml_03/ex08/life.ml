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

type aminoacid =
	| Ala
	| Arg
	| Asn
	| Asp
	| Cys
	| Gln
	| Glu
	| Gly
	| His
	| Ile
	| Leu
	| Lys
	| Met
	| Phe
	| Pro
	| Ser
	| Thr
	| Trp
	| Tyr
	| Val
	| Stop

type protein = aminoacid list

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

let rec generate_bases_triplets (mrna : rna) : (nucleobase * nucleobase * nucleobase) list =
	match mrna with
		| [] -> []
		| h :: i :: j :: t -> (h, i, j) :: generate_bases_triplets t
		| h :: t -> []

let decode_arn (mrna : rna) : protein =
	let get_aminoacid a =
		match a with
			| (U, U, U) | (U, U, C) -> Phe
			| (U, U, A) | (U, U, G) | (C, U, U) | (C, U, C) | (C, U, A) | (C, U, G) -> Leu
			| (A, U, U) | (A, U, C) | (A, U, A) -> Ile
			| (A, U, G) -> Met
			| (G, U, U) | (G, U, C) | (G, U, A) | (G, U, G) -> Val

			| (U, C, U) | (U, C, C) | (U, C, A) | (U, C, G) -> Ser
			| (C, C, U) | (C, C, C) | (C, C, A) | (C, C, G) -> Pro
			| (A, C, U) | (A, C, C) | (A, C, A) | (A, C, G) -> Thr
			| (G, C, U) | (G, C, C) | (G, C, A) | (G, C, G) -> Ala

			| (U, A, U) | (U, A, C) -> Tyr
			| (U, A, A) | (U, A, G) -> Stop
			| (C, A, U) | (C, A, C) -> His
			| (C, A, A) | (C, A, G) -> Gln
			| (A, A, U) | (A, A, C) -> Asn
			| (A, A, A) | (A, A, G) -> Lys
			| (G, A, U) | (G, A, C) -> Asp
			| (G, A, A) | (G, A, G) -> Glu

			| (U, G, U) | (U, G, C) -> Cys
			| (U, G, A) -> Stop
			| (U, G, G) -> Trp
			| (C, G, U) | (C, G, C) | (C, G, A) | (C, G, G) -> Arg
			| (A, G, U) | (A, G, C) -> Ser
			| (A, G, A) | (A, G, G) -> Arg
			| (G, G, U) | (G, G, C) | (G, G, A) | (G, G, G) -> Gly

			| (_, _, _) -> Stop
	in let rec loop l =
		match l with
			| [] -> [] 
			| h :: t when get_aminoacid h = Stop -> []
			| h :: t -> get_aminoacid h :: loop t
	in loop (generate_bases_triplets mrna)

let rec string_of_protein (p : protein) =
	let get_aminoacid a =
		match a with
			| Ala -> "Ala"
			| Arg -> "Arg"
			| Asn -> "Asn"
			| Asp -> "Asp"
			| Cys -> "Cys"
			| Gln -> "Gln"
			| Glu -> "Glu"
			| Gly -> "Gly"
			| His -> "His"
			| Ile -> "Ile"
			| Leu -> "Leu"
			| Lys -> "Lys"
			| Met -> "Met"
			| Phe -> "Phe"
			| Pro -> "Pro"
			| Ser -> "Ser"
			| Thr -> "Thr"
			| Trp -> "Trp"
			| Tyr -> "Tyr"
			| Val -> "Val"
			| Stop -> "Stop"
	in match p with
		| [] -> ""
		| h :: i :: t -> get_aminoacid h ^ "-" ^ string_of_protein (i :: t)
		| h :: t -> get_aminoacid h ^ string_of_protein t

let life str =
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
	in let get_triplet t =
		match t with
			| (a, b, c) -> get_nucleobase a ^ get_nucleobase b ^ get_nucleobase c
	in let rec triplets_to_string (l : (nucleobase * nucleobase * nucleobase) list) =
		match l with
			| [] -> ""
			| h :: t -> get_triplet h ^ " " ^ triplets_to_string t
	in let rec generate_helix_str str n (h : helix) : helix =
		if n < 0 then
			h
		else
			generate_helix_str str (n - 1) (generate_nucleotide (String.get str n) :: h)
	in if String.length str > 0 then begin
		let dna = generate_helix_str str ((String.length str) - 1) []
		in let dnac = complementary_helix dna;
		in let rna = generate_rna dna;
		in let tpl = generate_bases_triplets rna;
		in let prt = decode_arn rna;
		in print_endline ("str:    " ^ str);
		print_endline ("DNA:    " ^ (helix_to_string dna));
		print_endline ("        " ^ (helix_to_string dnac));
		print_endline ("RNA: AUG" ^ (rna_to_string rna));
		print_endline ("Triplets: AUG " ^ triplets_to_string(tpl));
		print_endline ("Protein:      " ^ string_of_protein (prt))
	end

let () =
	let random_char () =
		match Random.int 4 with
			| 0 -> "A"
			| 1 -> "T"
			| 2 -> "C"
			| 3 -> "G"
			| _ -> "N"
	in let rec random_str n l =
		if n <= 0 then
			l
		else
			random_str (n - 1) (random_char () ^ l)
	in Random.self_init ();
	life (random_str 10 "");
	print_char '\n';
	life (random_str 99 "");
	print_char '\n';
	life (random_str 66 "");
	print_char '\n';
	life (random_str (-1) "")