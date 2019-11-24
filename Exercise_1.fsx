let rec recursiveCount (list: int list, num: int) : int =
    match list with
    | [] -> 0
    | h::[] -> if h = num then 1 else 0
    | h::t -> (if h = num then 1 else 0) + recursiveCount (t,num)
        
let countFilter (list: int list, num: int) : int =
    list
    |> List.filter (fun e -> e = num)
    |> List.length
        
let countFold (list: int list, num: int) : int =
    list
    |> List.fold (fun acc e -> if e = num then acc + 1 else acc) 0