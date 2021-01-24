open Base

module Int_map = Map.M(Int)
type school = string list Int_map.t

let empty_school = Map.empty (module Int)

let add n g s =
    Map.change s g ~f:(fun v -> 
    match v with 
    | Some(t) -> Some(n::t)
    | None -> Some([n]))
    
let grade g s = 
    match Map.find s g with
    | Some(v) -> v
    | None -> []

let sorted s =
    Map.map ~f:( fun v -> List.sort ~compare:(String.compare) v) s
    
let roster s =
    sorted s
    |> Map.to_alist
    |> List.sort ~compare:(fun (k1,_) (k2, _) -> Int.compare k2 k1 )
    |> List.fold ~init:[] ~f:(fun accum (_, v) -> List.append v accum )
