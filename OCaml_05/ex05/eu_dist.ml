let eu_dist (a : float array) (b : float array) : float =
	let len a =
		Array.length a
	in if len a = len b then begin
		let calc a b =
			(a -. b) ** 2.0
		in let rec sum i n acc =
			if i > n then
				acc
			else
				sum (i + 1) n (acc +. calc a.(i) b.(i))
		in if Array.length a > 0 then
			sqrt (sum 0 ((len a) - 1) 0.0)
		else
			nan
	end
	else 
		nan

let () = 
	Printf.printf "%.4f\n" (eu_dist [|2.0; 2.0|] [|1.0; 1.0|]);
	Printf.printf "%.4f\n" (eu_dist [|-2.0; -2.0; -2.0|] [|1.0; 1.0; 1.0|]);
	Printf.printf "%.4f\n" (eu_dist [|3.4; 1.7|] [|3.9; 2.0|]);
	Printf.printf "%.4f\n" (eu_dist [|2.3; 4.2|] [|1.0|]);
