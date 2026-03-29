module type TRY =
sig
	type 'a t = Success of 'a | Failure of exn
	val return : 'a -> 'a t
	val bind : 'a t -> ('a -> 'b t) -> 'b t
	val recover : 'a t -> (exn -> 'a t) -> 'a t
	val filter : 'a t -> ('a -> bool) -> 'a t
	val flatten : 'a t t -> 'a t
end

module Try : TRY =
struct
	type 'a t = Success of 'a | Failure of exn

	let return (v : 'a) : 'a t =
		Success v

	let bind (v : 'a t) (f : ('a -> 'b t)) : 'b t =
		try
			match v with
				| Success a -> f a
				| Failure e -> Failure e
		with exn -> Failure exn

	let recover (v : 'a t) (f : (exn -> 'a t)) : 'a t =
		try
			match v with
				| Failure e -> f e
				| _ -> v
		with exn -> Failure exn

	let filter (v : 'a t) (f : ('a -> bool)) : 'a t =
		try
			match v with
				| Success a when not (f a) -> Failure (Failure "Predicate not satisfied")
				| _ -> v
		with exn -> Failure exn

	let flatten (v : 'a t t) : 'a t =
		match v with
			| Success a -> a
			| Failure e -> Failure e
end