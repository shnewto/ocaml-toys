let leap_year x = 
    match (x mod 4 == 0, x mod 100 == 0, x mod 400 == 0) with
	| (true, true, true ) -> true
	| (true, false, _) -> true
	| _     -> false
