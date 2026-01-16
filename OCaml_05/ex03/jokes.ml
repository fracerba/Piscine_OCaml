let loop file = 
	try
		let filein = 
			open_in file
		in let arr = 
			ref [||]
		in try
			while true do
				arr := Array.append !arr [|input_line filein|]
			done;
			!arr
		with End_of_file -> close_in filein;
		!arr
	with Sys_error err -> print_endline	("Error: " ^ err);
		[||]

let () =
	if Array.length Sys.argv = 2 then begin
		let jokes = 
			loop Sys.argv.(1)
		in if Array.length jokes <> 0 then begin
			Random.self_init();
			print_endline jokes.(Random.int (Array.length jokes));
		end
	end
	else
		print_endline "Error: invalid arguments"