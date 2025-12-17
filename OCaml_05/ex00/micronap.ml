let my_sleep () =
	Unix.sleep 1

let () =
	let limit n =
		int_of_string Sys.argv.(n)
	in if Array.length Sys.argv = 2 then begin
		try
			if limit 1 > 0 then
				for i = 0 to (limit 1) - 1 do
					my_sleep ()
				done
		with Failure _ -> ()
	end