let search target l =
  let rec _search target length original reversed left right = 
    let isSameIndex length left right = left = length - right - 1
    if left = length - right - 1 then
      None
    else
      match original with
      | x :: xs ->
        match reversed with
          | y :: ys -> 
            let curriedSearch = _search target length
            match x + y with
            | sum when sum = target -> Some(left, length - right - 1)
            | sum when sum > target -> curriedSearch original ys left (right + 1)
            | sum when sum < target -> curriedSearch xs reversed (left + 1) right
            | _ -> None
          | _ -> None

      | _ -> None
 
  _search target (List.length l) l (List.rev l) 0 0
 
let numbers = [1; 110; 200; 300; 400]
let target = 310
match search target numbers with
| Some(left, right) -> printfn "Found at index %d and %d yielding elements %d and %d" left right (List.item left numbers) (List.item right numbers)
| None -> printfn "Not found"