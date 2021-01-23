
let pling n = if n mod 3 = 0 then "Pling" else ""
let plang n = if n mod 5 = 0 then "Plang" else ""
let plong n = if n mod 7 = 0 then "Plong" else ""

let raindrop n =
    match String.concat "" [pling n; plang n; plong n] with 
    | "" -> string_of_int n
    | ret -> ret
