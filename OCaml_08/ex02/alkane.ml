class virtual alkane (nmb : int) =
  object (self)
    method name = 
      match nmb with
        | 1 -> "Methane"
        | 2 -> "Ethane"
        | 3 -> "Propane"
        | 4 -> "Butane"
        | 5 -> "Pentane"
        | 6 -> "Hexane"
        | 7 -> "Heptane"
        | 8 -> "Octane"
        | 9 -> "Nonane"
        | 10 -> "Decane"
        | 11 -> "Undecane"
        | 12 -> "Dodecane"
        | 13 -> "Tridecane"
        | 14 -> "Tetradecane"
        | 15 -> "Pentadecane"
        | 16 -> "Hexadecane"
        | _ -> "Unknown Alkane"
    method formula = "C" ^ string_of_int nmb ^ "H" ^ string_of_int (2 * nmb + 2)
    method to_string = self#name ^ " " ^ self#formula
    method equals (a : alkane) = self#name = a#name && self#formula = a#formula
  end

class methane =
  object
    inherit alkane 1
  end

class ethane =
  object
    inherit alkane 2
  end

class propane =
  object
    inherit alkane 3
  end

class butane =
  object
    inherit alkane 4
  end

class pentane =
  object
    inherit alkane 5
  end

class hexane =
  object
    inherit alkane 6
  end

class heptane =
  object
    inherit alkane 7
  end

class octane =
  object
    inherit alkane 8
  end

class nonane =
  object
    inherit alkane 9
  end

class decane = 
  object
    inherit alkane 10
  end

class undecane =
  object
    inherit alkane 11
  end

class dodecane =
  object
    inherit alkane 12
  end

class tridecane =
  object
    inherit alkane 13
  end

class tetradecane =
  object
    inherit alkane 14
  end

class pentadecane =
  object
    inherit alkane 15
  end

class hexadecane =
  object
    inherit alkane 16
  end