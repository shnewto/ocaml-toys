open Base 

let uppercaseable s = 
    match String.compare (String.uppercase s) (String.lowercase s) with 
    | 0 -> false
    | _ -> true 


let is_question s =
    match String.to_list s |> List.last with
    | Some('?') -> true
    | _ -> false

let is_yelling s =
    match (uppercaseable s, String.uppercase s |> String.compare s) with
    | (true, 0) -> true 
    | _ -> false

let is_silent s =
    match String.compare "" s with
    | 0 -> true 
    | _ -> false 

let response_for phrase =
    let p = String.strip phrase 
    in 
    if is_silent p then 
         "Fine. Be that way!"
    else 
    match (is_question p, is_yelling p) with 
    | (true, true) -> "Calm down, I know what I'm doing!"
    | (true, _) -> "Sure."
    | (_, true) -> "Whoa, chill out!"
    | _ -> "Whatever."
