module Sem2

// Exo 1 : tri à bulles
/// Compare consecutive values in rightlist, moving each time the lesser one to the leftlist
/// first call to bubbleToTop : leftlist = [], all elements in rightlist
/// after recursion ended : leftlist has all elements except the max which is in rightlist
let rec bubbleToTop (leftlist, rightlist) =
    match rightlist with
    | x :: y :: tail ->
        if x > y
        then bubbleToTop (leftlist @ [ y ], [ x ] @ tail)
        else bubbleToTop (leftlist @ [ x ], [ y ] @ tail)
    | _ -> (leftlist, rightlist)

let rec bubble (unsortedlist, sortedlist) =
    match unsortedlist with
    | [] -> (unsortedlist, sortedlist)
    | [ x ] -> ([], unsortedlist @ sortedlist)
    | _ ->
        let (leftlist, rightlist) = bubbleToTop ([], unsortedlist)
        bubble (leftlist, rightlist @ sortedlist)

let bubbleSort alist =
    let (_, res) = bubble (alist, [])
    res

// Tri à bulles amélioré
let rec bubbleToTopI (swapped, leftlist, rightlist) =
    match rightlist with
    | x :: y :: tail ->
        if x > y
        then bubbleToTopI (true, leftlist @ [ y ], [ x ] @ tail)
        else bubbleToTopI (swapped, leftlist @ [ x ], [ y ] @ tail)
    | _ -> (swapped, leftlist, rightlist)

let rec bubbleI (unsortedlist, sortedlist) =
    match unsortedlist with
    | [] -> (unsortedlist, sortedlist)
    | [ x ] -> ([], unsortedlist @ sortedlist)
    | _ ->
        let (swapped, leftlist, rightlist) = bubbleToTopI (false, [], unsortedlist)
        if swapped
        then bubbleI (leftlist, rightlist @ sortedlist)
        else ([], unsortedlist @ sortedlist)

let bubbleSortI alist =
    let (_, res) = bubbleI (alist, [])
    res

// Exo 2 : tri rapide
let rec quickSort inputlist =
    match inputlist with
    | [] -> inputlist
    | pivot :: tail ->
        let leftlist = List.filter (fun x -> x <= pivot) tail
        let rightlist = List.filter (fun x -> x > pivot) tail
        List.concat
            [ quickSort leftlist
              [ pivot ]
              quickSort rightlist ]

//  Tri rapide amélioré
let swapInArray (array: 'T []) i j =
    let oldi = array.[i]
    array.[i] <- array.[j]
    array.[j] <- oldi
    array

let rec quickSortI anarray =
    match anarray with
    | [||] -> anarray
    | [| x |] -> anarray
    | _ ->
        let pivot = anarray.[0]

        let temp pp i =
            match anarray.[i] with
            | x when x <= pivot ->
                swapInArray anarray (pp + 1) i |> ignore
                pp + 1
            | _ -> pp

        let p =
            List.fold temp 0 [ 1 .. Array.length anarray - 1 ]

        swapInArray anarray 0 p |> ignore
        Array.concat
            [ quickSortI anarray.[..(p - 1)]
              [| pivot |]
              quickSortI anarray.[(p + 1)..] ]

// Tests comparés
let rnd = System.Random()

let maliste =
    [ 0 .. 500 ]
    |> List.map (fun x -> rnd.Next(0, 10000))

// Sur une liste de 501 valeurs : bubble = 0.590s, bubbleI = 0.538s,
// quick = 0.005s, quickI = 0.004s
// #time
let bubbleSorted = bubbleSort maliste
// #time
// #time
let bubbleSortedI = bubbleSortI maliste
// #time
// #time
let quickSorted = quickSort maliste
// #time
// #time
let quickSortedI = quickSortI (List.toArray maliste)
// #time
