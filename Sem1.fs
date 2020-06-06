// Exercices cours 2I001

// * case against if-then-else and for/while : https://fsharpforfunandprofit.com/posts/control-flow-expressions/
// * timing exec time : use #time before and after he function to test
// Alternativement on peut utiliser System.Diagnostics.Stopwatch() dans le code de la fonction
// * RegEx : use open System.Text.RegularExpressions
// * function signature => val functionName : domain -> range
// * value signature (absence de ->) => val aName: type = constant
// * it indique la dernière valeur calculée => val it : int = 8
// * val printInt : int -> unit
//    unit est un équivalent du type void et il a une valeur unique ()
//    Exemple :
//    let whatIsThis = ()
//    val whatIsThis : unit = ()
// * Array, Array2D, Array3D, Array4D plus appropriés si tableaux = pavés et facilité d'accéder à index i, j, ....
// * explications de l'asynchrone : https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/asynchronous-and-concurrent-programming/async

module Sem1

open System

let isEven n = (n % 2 = 0)
let isOdd n = not (isEven n)

// Chap 1 exo 1
let leibnizSum n =
    let leibnizStep k = float (pown -1 k) / float (2 * k + 1)
    [ 0 .. n ] |> List.sumBy leibnizStep

let pi prec =
    match prec with
    | 0.0 -> None
    | p when p < 0.0 -> None
    | _ ->
        let res =
            4.0 * (leibnizSum (int (ceil (1.0 / prec - 1.5))))

        Some res

// Chap 1 exo 2
let fact n =
    let action acc k = acc * k
    match n with
    | neg when neg < 0 -> None
    | 0 -> Some 1
    | _ ->
        let res = [ 1 .. n ] |> List.fold action 1
        Some res

// Chap 1 exo 3
let myMinF (listF: float list) =
    let innermin acc x = if x < acc then x else acc
    match listF with
    | [] -> invalidArg "listF" "Argument de myMinF : liste vide" // alternative à None
    | first :: rest -> Some(List.fold innermin first rest)

let myMinFWithPos (listF: float list) =
    let innermin (acc: int * float * int) (x: float) =
        let (minInd, minVal, curInd) = acc
        if x < minVal then (curInd + 1, x, curInd + 1) else (minInd, minVal, curInd + 1)

    match listF with
    | [] -> invalidArg "listF" "Argument de myMinF : liste vide"
    | first :: rest ->
        let (resMinInd, resMinVal, resCurInd) = List.fold innermin (0, first, 0) rest
        Some(resMinInd, resMinVal)

// Chap 1 exo 4
let tabRandInt n maxVal =
    let getrand maxrand k =
        let randState = System.Random()
        int (randState.Next() % (maxrand + 1))

    List.init n (getrand maxVal)

let sommeTab list1 list2 = List.map2 (+) list1 list2

// Chap 1 exo 5
let rec pair n =
    match n with
    | 0 -> 1
    | _ -> impair (n - 1)

and impair n =
    match n with
    | 0 -> 0
    | _ -> pair (n - 1)

// Chap 1 exo 8
///////////////////////////////////////////////////
////////////////// LABYRINTHE /////////////////////
///////////////////////////////////////////////////
type SquareEnum =
    | Empty
    | Blocked
    | Test
    | Failed

type Maze = SquareEnum [,]

type MazeAndPos =
    { Maze: Maze
      RowNum: int
      ColNum: int }

type SquareCheck = { Exists: bool; State: SquareEnum }

let initMazeLine (line: string) =
    let charToSquare char =
        match char with
        | '0' -> SquareEnum.Empty
        | '1' -> SquareEnum.Blocked
        | _ -> invalidArg "line" "Attendu : chaine de 0 (non occupe) et de 1 (occupe)"

    line
    |> Seq.toList
    |> List.map charToSquare
    |> List.toArray

let initMaze mazeAsStringList: Maze =
    let lines =
        mazeAsStringList
        |> List.map initMazeLine
        |> List.toArray

    let height = lines.Length
    let lineLengths = lines |> Array.map Array.length

    let (widthmin, widthmax) =
        (Array.min lineLengths, Array.max lineLengths)

    match (widthmax - widthmin) with
    | 0 -> Array2D.init height widthmin (fun i j -> lines.[i].[j])
    | _ -> invalidArg "mazeAsStringList" "Attendu : des chaines de meme longueur"

let squareToString square =
    match square with
    | Empty -> " "
    | Blocked -> "*"
    | Test -> "2"
    | Failed -> "3"

let displayMaze (maze: Maze) =
    let length, width =
        Array2D.length1 maze, Array2D.length2 maze

    let squareFormat i j =
        match i % (length + 1), j % (width + 1) with
        | 0, 0 -> "+"
        | 0, _ -> "-"
        | _, 0 -> "|"
        | _, _ -> squareToString maze.[i - 1, j - 1]

    let output =
        Array2D.init (length + 2) (width + 2) squareFormat
        |> Seq.cast<string>
        |> Seq.chunkBySize (width + 2)
        |> Seq.map (Array.reduce (+))
        |> Seq.toList

    List.map (printfn "%s") output

let getState (maze: Maze) i j =
    match (i / (Array2D.length1 maze)), (j / (Array2D.length2 maze)) with
    | 0, 0 -> maze.[i, j]
    | _, _ -> Blocked

let setState (maze: Maze) (state: SquareEnum) i j =
    match (i / (Array2D.length1 maze)), (j / (Array2D.length2 maze)) with
    | 0, 0 -> maze.[i, j] <- state
    | _, _ -> invalidArg "i, j" "Hors des limites du labyrinthe"

let findPath (maze: Maze) =
    let rec subPath smaze i j =
        let maxi, maxj =
            (Array2D.length1 smaze) - 1, (Array2D.length2 smaze) - 1

        let state = getState smaze i j
        match state with
        | Blocked -> false
        | Test -> false
        | Failed -> false
        | _ ->
            setState smaze Test i j
            match (i, j) with
            | exit when exit = (maxi, maxj) -> true
            | _ ->
                let around =
                    subPath smaze i (j + 1)
                    || subPath smaze (i + 1) j
                    || subPath smaze i (j - 1)
                    || subPath smaze (i - 1) j

                match around with
                | true -> true
                | _ ->
                    setState smaze Failed i j |> ignore
                    false

    subPath maze 0 0

let main =
    let myList =
        [ "00100011"
          "10011010"
          "10111000"
          "10010011"
          "01000101"
          "00010101"
          "11110000"
          "10110000" ]

    let myMaze = initMaze myList
    displayMaze myMaze |> ignore
    let found = findPath myMaze
    printfn "found=%A" found
    displayMaze myMaze |> ignore
    0
