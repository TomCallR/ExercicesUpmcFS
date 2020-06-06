module Params =

    let MinX = 1
    let MinY = 1
    let SizeX = 10
    let SizeY = 10
    let MinDir = -1
    let MaxDir = 1
    let Energy = 1.0
    let ChangeDir = 0.2
    let Reproduce = 0.2
    let ChangeEnerPrey = -0.1
    let ChangeEnerPred = -0.2    
    let ChanceToEat = 0.33
    let RandState = System.Random()

module Pos =

    type TPos = Pos of x:int * y:int
    let create (x:int) (y:int) =
        let moduloX = Params.SizeX - Params.MinX + 1
        let moduloY = Params.SizeY - Params.MinY + 1
        // modulo of negative numbers wrong : so adding moduloZ
        let newx = (x - Params.MinX + moduloX) % moduloX + Params.MinX
        let newy = (y - Params.MinY + moduloY) % moduloY + Params.MinY
        Pos (newx, newy)
    let unwrap (Pos (x, y)) = (x, y)

module Dir =

    type TDir = Dir of dx:int * dy:int
    let create (dx:int) (dy:int) =
        let modulo = Params.MaxDir - Params.MinDir + 1
        let newdx = (dx - Params.MinDir) % modulo + Params.MinDir
        let newdy = (dy - Params.MinDir) % modulo + Params.MinDir
        Dir (newdx, newdy)
    let createRandom() =
        let newval() = Params.RandState.Next(Params.MinDir, Params.MaxDir + 1)
        create (newval()) (newval())
    let unwrap (Dir (dx, dy)) = (dx, dy)

type Animal =
    | Prey of pos: Pos.TPos * dir: Dir.TDir * energy: float
    | Predator of pos: Pos.TPos * dir: Dir.TDir * energy: float
    | Dead of pos: Pos.TPos

let createAnimal posx posy =
    (Pos.create posx posy, Dir.createRandom(), Params.Energy)

let createPrey posx posy =
    Prey (createAnimal posx posy)

let createPredator posx posy =
    Predator (createAnimal posx posy)

let unwrapAnimal animal =
    match animal with
    | Prey (p, d, e) -> (p, d, e)
    | Predator (p, d, e) -> (p, d, e)
    | Dead (p) -> (p, Dir.create 0 0, -1.)

let isPrey (animal:Animal) =
    match animal with
    | Prey _ -> true
    | _ -> false

let isPredator (animal:Animal) =
    match animal with
    | Predator _ -> true
    | _ -> false

let countAnimals anilist =
    let counter (cpreys, cpreds, cdead) a =
        match a with
        | p1 when (isPrey a) -> (cpreys + 1, cpreds, cdead)
        | p2 when (isPredator a) -> (cpreys, cpreds + 1, cdead)
        | _ ->  (cpreys, cpreds, cdead + 1)
    anilist
    |> List.fold counter (0, 0, 0)

let isDead animal =
    not (isPrey animal || isPredator animal)

let getPos animal =
    match animal with
    | Prey (pos, _, _) -> pos
    | Predator (pos, _, _) -> pos
    | Dead (pos) -> pos

let getAnimalsByPos anilist =
    anilist
    |> Seq.groupBy getPos
    |> Seq.map (fun (pos, sq) -> (pos, List.ofSeq sq))
    |> List.ofSeq

let getAnimalsByPosAndType anilist =
    getAnimalsByPos anilist
    |> List.map (fun (pos, ll) -> (pos, List.filter isPrey ll, List.filter isPredator ll))

let getStatsByPosAndType anilist =
    getAnimalsByPosAndType anilist
    |> List.map (fun (pos, lpreys, lpreds) -> (pos, (List.length lpreys, List.length lpreds)))

let getOutputByStats stats =
    match stats with
    | (0, 0) -> " "
    | (0, _) -> "O"
    | (_, 0) -> "*"
    | _ -> "@"

let displayAnimalMap anilist =
    let getAnimalsCountByPos j i =
        let thisPosStats =
            getStatsByPosAndType anilist
            |> Map.ofList
            |> Map.tryFind (Pos.create i j)
        match thisPosStats with
        | Some tup -> tup
        | None -> (0, 0)
    let buildRow j =
        [1..Params.SizeX]
        |> List.map (getAnimalsCountByPos j >> getOutputByStats)
        |> List.reduce (fun acc x -> acc + "|" + x)
    let linesToDisplay = 
        [1.. Params.SizeY]
        |> List.map buildRow
    let rule = (String.replicate ((Params.SizeX * 2) + 1) "-")
    printfn "%s" rule
    linesToDisplay |> List.map (printfn "|%s|") |> ignore
    printfn "%s" rule
    anilist

let moveAnimal animal =
    let computeNewPos pos dir =
        let (x, y) = Pos.unwrap pos
        let (dx, dy) = Dir.unwrap dir
        Pos.create (x + dx) (y + dy)
    let computeNewDir dir =
        match Params.RandState.NextDouble() < Params.ChangeDir with
        | true -> Dir.createRandom()
        | _ -> dir
    match animal with
    | Prey (p, d, e) -> Prey (computeNewPos p d, computeNewDir d, e)
    | Predator (p, d, e) -> Predator (computeNewPos p d, computeNewDir d, e)
    | Dead (p) -> Dead (p)

let moveAnimalsOnCondition cond anilist =
    anilist
    |> List.map (fun a -> if (cond a) then (moveAnimal a) else a)

let reproduce animal =
    match Params.RandState.NextDouble() < Params.Reproduce with
    | true ->
        match animal with
        | Prey (p, _, _) -> [createPrey (fst (Pos.unwrap p)) (snd (Pos.unwrap p))]
        | Predator (p, _, _) -> [createPredator (fst (Pos.unwrap p)) (snd (Pos.unwrap p))]
        | Dead _ -> []
    | false -> []

let reproduceOnCondition cond anilist =
    let newAnimals = anilist |> List.collect (fun a -> if cond a then reproduce a else [])
    List.concat [newAnimals; anilist]

let decreaseEnergy animal =
    match animal with
    | Prey (p, d, e) ->
        let newenergy = e + Params.ChangeEnerPrey
        if newenergy > 0. then Prey (p, d, newenergy) else Dead (p)
    | Predator (p, d, e) ->
        let newenergy = e + Params.ChangeEnerPred
        if newenergy > 0. then Predator (p, d, newenergy) else Dead (p)
    | Dead _ -> animal

let decreaseEnergyOnCondition cond anilist =
    anilist
    |> List.map (fun a -> if (cond a) then (decreaseEnergy a) else a)

let getFirstPreyByPos anilist pos =
    let (validPreys, others) =
        anilist
        |> List.partition (fun a -> (isPrey a) && ((getPos a) = pos))
    match validPreys with
    | [] -> (None, anilist)
    | first::tail -> (Some (first), tail @ others)

let feedPredators anilist =
    let choosePredsFed preds =
        preds
        |> List.partition (fun a -> Params.RandState.NextDouble() < Params.ChanceToEat)
    let rec feed (inpreds, inpreys, outpreds, outpreys) =
        match (inpreds, inpreys) with
        | [], _ -> inpreds, inpreys
        | _, [] -> inpreds, inpreys
        | firstpred::otherpreds, firstprey::otherpreys ->
            let (p1, d1, e1) = unwrapAnimal firstpred
            let (p2, _, e2) = unwrapAnimal firstprey
            let newfirstpred = Predator (p1, d1, e1 + e2)
            let newfirstprey = Dead (p2)
            feed (otherpreds, otherpreys, newfirstpred::outpreds, newfirstprey::outpreys)
    let feedPredsByPos (pos, preys, preds) =
        match preys, preds with
        | [], _ -> (pos, preys, preds)
        | _, [] -> (pos, preys, preds)
        | _ ->
            let (predsTooFeed, predsNotFed) = choosePredsFed preds
            let (predsAfterMeal, preysAfterMeal) = feed (predsTooFeed, preys, [], [])
            (pos, preysAfterMeal, predsAfterMeal @ predsNotFed)
    getAnimalsByPosAndType anilist
    |> List.map feedPredsByPos
    |> List.collect (fun (pos, preys, preds) -> preys @ preds)

let cycleThroughList anilist =
    anilist
    |> moveAnimalsOnCondition isPrey
    |> decreaseEnergyOnCondition isPrey
    |> reproduceOnCondition isPrey
    |> moveAnimalsOnCondition isPredator
    |> decreaseEnergyOnCondition isPredator
    |> feedPredators
    |> reproduceOnCondition isPredator

let rec gameloop anilist =
    let newlist = 
        anilist
        |> cycleThroughList
        |> displayAnimalMap
    printfn "(preys, preds, dead) : %A" (countAnimals newlist)
    System.Threading.Thread.Sleep(2000)
    printfn "Continuer ? q pour quitter"
    let input = System.Console.ReadKey()
    match input.Key with
    | System.ConsoleKey.Q -> newlist
    | _ -> gameloop newlist

// Main
// https://stackoverflow.com/questions/10178706/infinite-main-loop-in-f
[<EntryPoint>]
let main argv =

    let (preycnt, predcnt) =
        match argv with
        | [||] -> (10, 3)
        | [|x|] -> (int x, int x)
        | _ -> (int argv.[0], int argv.[1])
    let preys =
        [1..preycnt]
        |> List.map (fun x -> createPrey (Params.RandState.Next(999999)) (Params.RandState.Next(999999)))
    let preds =
        [1..predcnt]
        |> List.map (fun x -> createPredator (Params.RandState.Next(999999)) (Params.RandState.Next(999999)))
    let animals = List.concat [preys; preds]
    printfn "-- Etat initial --"
    displayAnimalMap animals |> ignore
    printfn "(preys, preds, dead) : %A" (countAnimals animals)
    // Game loop
    animals
    |> gameloop
    |> ignore
    0