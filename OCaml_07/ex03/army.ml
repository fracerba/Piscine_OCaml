class ['a] army (army : 'a list) =
	object (self)
		val members : 'a list = army
		method add x = {< members = x :: members >}
		method delete = 
			match members with
				| [] -> self
				| h :: t -> {< members = t >}
		method get_members = members
	end