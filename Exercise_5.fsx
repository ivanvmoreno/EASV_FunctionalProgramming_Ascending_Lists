let rec minus (a: int list, b: int list) : int list =
    let minus' a b =
        match a, b with
        | x::a', y::b' ->
            if x < y then x :: minus a' b
            elif x = y then    minus a' b'
            else               minus a b'
        | a, [] -> a
        | _ -> []
    minus' a b