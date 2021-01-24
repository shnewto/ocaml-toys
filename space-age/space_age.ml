open Base 
type planet = Mercury | Venus | Earth | Mars
            | Jupiter | Saturn | Neptune | Uranus

let seconds_to_earth_years seconds = 
    Float.(/) (Int.to_float seconds) 31557600.

let age_on place seconds =
    match place with 
    | Earth -> seconds_to_earth_years seconds
    | Mercury -> Float.(/) (seconds_to_earth_years seconds) 0.2408467 
    | Venus -> Float.(/) (seconds_to_earth_years seconds) 0.61519726
    | Mars -> Float.(/) (seconds_to_earth_years seconds) 1.8808158
    | Jupiter -> Float.(/) (seconds_to_earth_years seconds) 11.862615
    | Saturn -> Float.(/) (seconds_to_earth_years seconds) 29.447498
    | Uranus -> Float.(/) (seconds_to_earth_years seconds) 84.016846 
    | Neptune -> Float.(/) (seconds_to_earth_years seconds) 164.79132
