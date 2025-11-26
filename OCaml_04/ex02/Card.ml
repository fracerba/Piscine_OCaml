module Color = struct
	type t = Spade | Heart | Diamond | Club

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
end

module Value = struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all : t list = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

	let toInt t : int =
		match t with
			| T2 -> 1
			| T3 -> 2
			| T4 -> 3
			| T5 -> 4
			| T6 -> 5
			| T7 -> 6
			| T8 -> 7
			| T9 -> 8
			| T10 -> 9
			| Jack -> 10
			| Queen -> 11
			| King -> 12
			| As -> 13

	let toString t : string =
		match t with
			| T2 -> "2"
			| T3 -> "3"
			| T4 -> "4"
			| T5 -> "5"
			| T6 -> "6"
			| T7 -> "7"
			| T8 -> "8"
			| T9 -> "9"
			| T10 -> "10"
			| Jack -> "J"
			| Queen -> "Q"
			| King -> "K"
			| As -> "A"

	let toStringVerbose t : string =
		match t with
			| T2 -> "2"
			| T3 -> "3"
			| T4 -> "4"
			| T5 -> "5"
			| T6 -> "6"
			| T7 -> "7"
			| T8 -> "8"
			| T9 -> "9"
			| T10 -> "10"
			| Jack -> "Jack"
			| Queen -> "Queen"
			| King -> "King"
			| As -> "As"

	let next t : t =
		match t with
			| T2 -> T3
			| T3 -> T4
			| T4 -> T5
			| T5 -> T6
			| T6 -> T7
			| T7 -> T8
			| T8 -> T9
			| T9 -> T10
			| T10 -> Jack
			| Jack -> Queen
			| Queen -> King
			| King -> As
			| As -> invalid_arg "Value.next: no next for As"

	let previous t : t =
		match t with
			| T2 -> invalid_arg "Value.previous: no previous for 2"
			| T3 -> T2
			| T4 -> T3
			| T5 -> T4
			| T6 -> T5
			| T7 -> T6
			| T8 -> T7
			| T9 -> T8
			| T10 -> T9
			| Jack -> T10
			| Queen -> Jack
			| King -> Queen
			| As -> King
end

type t = Value.t * Color.t

let newCard (v : Value.t) (c : Color.t) : t =
	(v, c)

let allSpades : t list =
	let apply_color (a : Value.t) = 
		(a, Color.Spade)
	in List.map apply_color Value.all

let allHearts : t list =
	let apply_color (a : Value.t) = 
		(a, Color.Heart)
	in List.map apply_color Value.all

let allDiamonds : t list =
	let apply_color (a : Value.t) = 
		(a, Color.Diamond)
	in List.map apply_color Value.all

let allClubs : t list =
	let apply_color (a : Value.t) = 
		(a, Color.Club)
	in List.map apply_color Value.all

let all : t list = allSpades @ allHearts @ allDiamonds @ allClubs

let getValue (c : t) : Value.t =
	fst c

let getColor (c : t) : Color.t =
	snd c

let toString (c : t) : string =
	Value.toString (fst c) ^ Color.toString (snd c)

let toStringVerbose (c : t) : string =
	"Card(" ^ (Value.toStringVerbose (fst c)) ^ ", " ^ (Color.toStringVerbose (snd c)) ^ ")"

let compare (a : t) (b : t) : int =
	if a > b then
		1
	else if a = b then
		0
	else
		-1

let max (a : t) (b : t) : t =
	if (Value.toInt (fst a)) < (Value.toInt (fst b)) then
		b
	else
		a

let min (a : t) (b : t) : t =
	if (Value.toInt (fst a)) > (Value.toInt (fst b)) then
		b
	else
		a

let best (l : t list) : t =
	match l with 
		| [] -> invalid_arg "Card.best: empty list"
		| h :: t -> List.fold_left max h t

let isOf (c : t) (clr : Color.t) : bool =
	snd c = clr

let isSpade (c : t) : bool =
	snd c = Color.Spade

let isHeart (c : t) : bool =
	snd c = Color.Heart

let isDiamond (c : t) : bool =
	snd c = Color.Diamond

let isClub (c : t) : bool =
	snd c = Color.Club