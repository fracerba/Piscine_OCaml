let rec repeat_string str n =
	let s = 
		if n < 0 then
			"Error"
		else if n > 0 then begin
			if str then
				str ^ repeat_string str (n - 1)
			else
				"x" ^ repeat_string (n - 1)
		end
		else
			""

let () =