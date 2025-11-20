type t = 
	| Spade 
	| Heart 
	| Diamond 
	| Club

let all : t list = [Spade; Heart; Diamond; Club]

let toString t : string =
	match t with
		| Spade -> "S"
		| Heart -> "H"
		| Diamond -> "D"
		| Club -> "C"

let toStringVerbose t : string =
	match t with
		| Spade -> "Spade"
		| Heart -> "Heart"
		| Diamond -> "Diamond"
		| Club -> "Club"