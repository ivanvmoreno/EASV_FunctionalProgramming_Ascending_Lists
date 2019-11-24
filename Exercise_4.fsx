let rec recursiveInsert (list: int list, num: int) : int list =
    match list with
    | [] -> [num]
    | h::[] -> if h < num then [h;num] else [num;h]
    | h::t when h >= num -> [num;h] @ t
    | h::t when h < num ->  h::(recursiveInsert (t, num))
    | _ -> failwith "Incomplete pattern matching"

let plus (a: int list, b: int list) : int list =
    let rec plus' xs ys =    
        match xs with    
        | x::xs' -> recursiveInsert (ys,x) |> plus' xs'
        | [] -> ys
        | _ -> failwith "Incomplete pattern matching"
    plus' a b