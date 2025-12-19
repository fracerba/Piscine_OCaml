let my_sleep () = Unix.sleep 1

let () =
	if Array.length Sys.argv = 2 then begin
		try
			let limit =
				int_of_string Sys.argv.(1)
			in if limit > 0 then
				for i = 0 to limit - 1 do
					my_sleep ()
				done
		with Failure _ -> ()
	end