open Base 

type allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats

let allergen_eq a b = 
    match a, b with
    | Eggs, Eggs -> true 
    | Peanuts, Peanuts -> true 
    | Shellfish, Shellfish -> true 
    | Strawberries, Strawberries -> true 
    | Tomatoes, Tomatoes -> true 
    | Chocolate, Chocolate -> true 
    | Pollen, Pollen -> true 
    | Cats, Cats -> true 
    | _ -> false 

let allergen_map = 
    Map.of_alist_exn (module Int) 
    [
    (1, Eggs); 
    (2, Peanuts); 
    (4, Shellfish); 
    (8, Strawberries); 
    (16, Tomatoes); 
    (32, Chocolate); 
    (64, Pollen); 
    (128, Cats);
    ]
    
let allergies i =
    if i = 0 then [] else 
    let (_, res) = 
        Map.fold_right allergen_map 
        ~init:(i, []) 
        ~f:(fun ~key:k ~data:v (count, acc) -> if count >= k then (count - k, v::acc) else (count, acc))
    in 
    res

let allergic_to i a =
    match allergies i |> List.find ~f:(fun b -> allergen_eq a b) with
    | Some(_) -> true
    | _ -> false 


