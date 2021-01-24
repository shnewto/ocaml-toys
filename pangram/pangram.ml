open Base

let alphabet = 
    "abcdefghijklmnopqrstuvwxyz"

let is_pangram s =
    String.strip s
    |> String.lowercase
    |> String.to_list 
    |> List.dedup_and_sort ~compare:(Char.compare) 
    |> String.of_char_list
    |> String.is_substring ~substring:alphabet

    