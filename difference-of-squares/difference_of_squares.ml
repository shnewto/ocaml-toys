let rec sum_range acc x n = 
    if x = n + 1 then 
        acc
    else 
        sum_range (acc + x) (x + 1) n

let rec square_range acc x n = 
    if x = n + 1 then 
        acc
    else 
        square_range (acc + (x * x)) (x + 1) n

let square_of_sum n = 
    (sum_range 0 1 n) * (sum_range 0 1 n)

let sum_of_squares n =
    square_range 0 1 n 

let difference_of_squares n =
    (square_of_sum n) - (sum_of_squares n)
