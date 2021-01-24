open Base

let empty = Base.Map.empty (module Base.Char)
let valid = ['A'; 'C'; 'G'; 'T'] 
let is_valid_char c = 
  List.find ~f:(fun a -> Char.equal a c) valid


let get_one_invalid_dna_char s = 
  let invalid_chars =
    String.filter s ~f:(fun c -> if is_valid_char c |> Option.is_none then true else false)
    |> String.to_list
  in
  match invalid_chars with
  | [] -> None
  | h::_ -> Some(h)


let count_nucleotide s c = 
  if is_valid_char c |> Option.is_none then
    Error c
  else
    match get_one_invalid_dna_char s with
    | Some(e) -> Error e
    | None -> 
      if String.equal s "" then
        Ok 0
      else 
        Ok (String.to_list s |> List.fold ~f:(fun acc n -> if Char.equal n c then acc + 1 else acc) ~init:0)

let get_errors a = 
  List.fold ~init:[] ~f:(fun acc v -> match v with | (_, Error(h)) -> List.append [h] acc | _ -> acc ) a
let get_unique_oks a = 
  let with_dups = 
    List.fold ~init:[] ~f:(fun acc v -> match v with | (c, Ok(h)) ->  List.append [(c, h)] acc | _ -> acc ) a
  in
    List.dedup_and_sort ~compare:(fun (c1, _) (c2, _) -> Char.compare c1 c2) with_dups
  
  
let count_nucleotides s = 
  if String.equal s "" then 
    Ok empty
  else 
  let res = Base.String.to_list s |> List.map ~f:(fun c -> (c, (count_nucleotide s c)))
  in
  match get_errors res with 
  | h::_ -> Error h
  | _ ->  Ok (get_unique_oks res |> Map.of_alist_exn (module Char))
    

