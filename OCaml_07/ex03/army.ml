class ['a] army (army : 'a list) =
	object (self)
		val members : 'a list = army
		method add x = x :: members
		method delete = 
			match members with
				| [] -> []
				| h :: t -> t
end