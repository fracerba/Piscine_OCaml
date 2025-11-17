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

let print_nucleotide n =
	print_string ("{phosphate = \"" ^ n.phosphate ^ "\"; ");
	print_string ("deoxyribose = \"" ^ n.deoxyribose ^ "\"; ");
	print_string ("nucleobase =  ");
	match n.nucleobase with
		| A -> print_endline "A}"
		| T -> print_endline "T}"
		| C -> print_endline "C}"
		| G -> print_endline "G}"
		| None -> print_endline "None}"

let () =
	print_nucleotide (generate_nucleotide 'A');
	print_nucleotide (generate_nucleotide 'T');
	print_nucleotide (generate_nucleotide 'C');
	print_nucleotide (generate_nucleotide 'G');
	print_nucleotide (generate_nucleotide 'N');
	print_nucleotide (generate_nucleotide 'O')