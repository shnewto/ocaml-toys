type nucleotide = A | C | G | T

let hamming_distance a b = 
    match (a, b) with
    | ([], _::_) -> Error "left strand must not be empty"
    | (_::_, []) -> Error "right strand must not be empty"
    | _ -> 
        if List.length a <> List.length b then
            Error "left and right strands must be of equal length" 
        else 
            Ok(List.fold_left (fun acc (x, y) -> if x <> y then acc + 1 else acc) 0 (List.combine a b))
            

