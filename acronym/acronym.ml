open Base 

let ascii_word_chars = 
    "abcdefghijklmnopqrstuvwxyz'"

let acronym string =
    String.lowercase string
    |> String.to_list 
    |> List.map ~f:(fun c -> if String.contains ascii_word_chars c then c else ' ')
    |> String.of_char_list
    |> String.split ~on:' '
    |> List.filter ~f:(fun s -> String.strip s |> String.is_empty |> not)
    |> List.map ~f:(fun s -> String.get s 0)
    |> String.of_char_list
    |> String.uppercase