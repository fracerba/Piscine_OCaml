module type SET =
sig
	type 'a t
	val return : 'a -> 'a t
	val bind : 'a t -> ('a -> 'b) -> 'b t
	val union : 'a t -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val filter : 'a t -> ('a -> bool) -> 'a t
	val foreach : 'a t -> ('a -> unit) -> unit
	val for_all : 'a t -> ('a -> bool) -> bool
	val exists : 'a t -> ('a -> bool) -> bool
end

module Set : SET =
struct
	type 'a t = 'a list

	let return (e : 'a) : 'a t =
		[e]

	let bind (a : 'a t) (f : ('a -> 'b)) : 'b t =
		let rec loop a acc =
			match a with
				| [] -> acc
				| h :: t when List.mem (f h) acc -> loop t acc
				| h :: t -> loop t (f h :: acc)
		in loop a []

	let union (a : 'a t) (b : 'a t) : 'a t =
		let rec loop a b acc =
			match a, b with
				| [], [] -> acc
				| [], h :: t -> loop [] t (h :: acc)
				| h :: t, [] -> loop t [] (h :: acc)
				| h1 :: t1, h2 :: t2 when h1 = h2 -> loop t1 t2 (h1 :: acc)
				| h1 :: t1, h2 :: t2 when h1 < h2 -> loop t1 b (h1 :: acc)
				| h1 :: t1, h2 :: t2 -> loop a t2 (h2 :: acc)
		in loop a b []

	let inter (a : 'a t) (b : 'a t) : 'a t =
		let rec loop a b acc =
			match a with
				| [] -> acc
				| h :: t when List.mem h b -> loop t b (h :: acc)
				| h :: t -> loop t b acc
		in loop a b []

	let diff (a : 'a t) (b : 'a t) : 'a t =
		let check h acc =
			not (List.mem h acc)
		in let rec loop a b acc =
			match a, b with
				| [], [] -> acc
				| [], h :: t when check h acc -> loop [] t (h :: acc)
				| h :: t, [] when check h acc -> loop t [] (h :: acc)
				| [], h :: t -> loop [] t acc
				| h :: t, [] -> loop t [] acc
				| h1 :: t1, h2 :: t2 when h1 < h2 && check h1 acc -> loop t1 b (h1 :: acc)
				| h1 :: t1, h2 :: t2 when h1 > h2 && check h2 acc -> loop a t2 (h2 :: acc)
				| h1 :: t1, h2 :: t2 -> loop t1 t2 acc
		in loop a b []

	let filter (a : 'a t) (f : ('a -> bool)) : 'a t =
		List.filter f a

	let foreach (a : 'a t) (f : ('a -> unit)) : unit =
		List.iter f a

	let for_all (a : 'a t) (f : ('a -> bool)) : bool =
		List.for_all f a

	let exists (a : 'a t) (f : ('a -> bool)) : bool =
		List.exists f a
end