let rec intersect (a: int list, b: int list) : int list =
    let intersect' a b =
        match a, b with
        | x::a', y::b' ->
            if   x = y then x :: intersect a' b'
            elif x < y then      intersect a' b
            else                 intersect a  b'
        | _ -> []
    intersect' a b