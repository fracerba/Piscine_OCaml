let rec converges f x n =
	if n < 0 then
		false
	else if f x = x then
			true
	else
		converges f (f x) (n - 1)

let () =
	if converges (( * ) 2) 2 5 then 
		print_endline "true"
	else
		print_endline "false";

  if converges (fun x -> x / 2) 2 3  then 
		print_endline "true"
	else
		print_endline "false";

  if converges (fun x -> x / 2) 2 2  then 
		print_endline "true"
	else
		print_endline "false"