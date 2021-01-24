open Base 

let sorted_str s = 
    String.lowercase s
    |> String.to_list 
    |> List.sort ~compare:(Char.compare)
    |> String.of_char_list

let strs_anagrams a b = 
    String.compare (sorted_str a) (sorted_str b)
    |> (=) 0 

let case_insensitve_equal a b = 
    String.equal (String.lowercase a) (String.lowercase b)

let anagrams x xs =
    List.fold xs ~init:[] 
    ~f:(fun acc h -> 
            if case_insensitve_equal h x  then acc 
            else if strs_anagrams x h then h::acc 
            else acc
    )
    |> List.rev

