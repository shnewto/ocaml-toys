let zero_len_sides a b c = 
    a = 0 || b == 0 || c == 0

let invalid_side_lens a b c = 
    zero_len_sides a b c || (a + b) < c || (b + c) < a || (c + a) < b

let is_equilateral a b c =
    if invalid_side_lens a b c then false 
    else 
    (a = b && b = c)

let is_isosceles a b c =
    if invalid_side_lens a b c then false 
    else 
    match a = b, b = c, c = a with
    | true, _, _ -> true 
    | _, true, _ -> true 
    | _, _, true -> true 
    | _ -> false 

let is_scalene a b c =
    if invalid_side_lens a b c then false 
    else 
    (a <> b) && (a <> c) && (c <> b)