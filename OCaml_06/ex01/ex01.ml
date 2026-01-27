module StringHashtbl = Hashtbl.Make(String)

let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
		List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
		StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht