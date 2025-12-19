type 'a ft_ref = {
	mutable contents : 'a;
}

let return (a : 'a) : 'a ft_ref = 
	{
		contents = a
	}

let get (a : 'a ft_ref) : 'a =
	a.contents

let set (a : 'a ft_ref) (b : 'a) = 
	a.contents <- b

let bind (a : 'a ft_ref) (f : 'a -> 'b ft_ref) : 'b ft_ref =
	f a.contents

let () = 
	let f a =
		return ("\"" ^ (string_of_int a) ^ "\"")
	in let ref_a =
		return 41
	in Printf.printf "%i\n" (get ref_a);

	set ref_a 23;
	Printf.printf "%i\n" (get ref_a);

	Printf.printf "%s\n" (get (bind ref_a f))
	
