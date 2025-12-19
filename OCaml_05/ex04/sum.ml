let sum a b =
	a +. b

let () =
	Printf.printf "%.1f\n" (sum 1. 22.);
	Printf.printf "%.1f\n" (sum (-2.8) 4.2);
	Printf.printf "%.2f\n" (sum 10.87 31.12);
	Printf.printf "%.4f\n" (sum 6748.7585 5638.5689);
	Printf.printf "%.1f\n" (sum 10.87 nan);
	Printf.printf "%.1f\n" (sum max_float (-.min_float))
