open Base

let transform_individual (value, chars) = 
    chars
    |> List.map ~f:(fun c -> (Char.lowercase c, value)) 

let transform legacy =
    legacy
    |> List.concat_map ~f:transform_individual 
    |> List.dedup_and_sort ~compare:(fun (c1, _) (c2, _) -> Char.compare c1 c2)