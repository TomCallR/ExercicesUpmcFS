// Réf https://en.wikipedia.org/wiki/Left-child_right-sibling_binary_tree
// This type of tree is a LCRS tree (left child - right sibling)

module Params =
    let wordsFile = "./french_modifie"

module Seq =

    let initSeqDict =
        seq {
            use reader = new System.IO.StreamReader(Params.wordsFile)
            while not reader.EndOfStream do
                yield reader.ReadLine()
        }
    let subDict dict startWord =
        Seq.skipWhile (fun w -> w <> startWord) dict

module BinTree =

    type Tree =
        | Empty
        | Node of Node

    and Node =
        { Char: char
          WordEnd: bool
          Son: Tree
          Bro: Tree }

    let rec cata fEmpty fNode tree: 'r =
        let recurse = cata fEmpty fNode
        match tree with
        | Empty -> fEmpty
        | Node node ->
            fNode node
            recurse node.Son |> ignore
            recurse node.Bro

    // Not tail recursive
    let rec fold fEmpty fNode tree acc: 'r =
        let recurse = fold fEmpty fNode
        match tree with
        | Empty -> fEmpty acc
        | Node node ->
            let nodeacc = fNode acc node
            recurse node.Son nodeacc |> ignore
            recurse node.Bro acc

     // See https://stackoverflow.com/questions/16875835/fold-recursion-over-multiway-tree-in-f/16876066#16876066
    // (Thomas Petricek) for details
    let foldback fEmpty fNode fCombine tree: 'r =
        let rec recurse tr cont =
            match tr with
            | Empty -> cont fEmpty
            | Node node ->
                recurse node.Son (fun ts ->
                    let resSon = fNode node ts
                    recurse node.Bro (fun tb ->
                        cont (fCombine resSon tb)))
        recurse tree id

    // Add word
    let addWord tree word =
        let rec recurse tr lw =
            match lw with
            | [] -> tr
            | c :: tail ->
                let wordend = tail.IsEmpty
                match tr with
                | Empty ->
                    let son = recurse Empty tail
                    Node { Char = c; WordEnd = wordend; Son = son; Bro = Empty }
                | Node node ->
                    if node.Char = c then
                        match wordend, node.WordEnd with
                        | true, true -> tr
                        | true, false -> Node { node with WordEnd = true }
                        | _ ->
                            let son = recurse node.Son tail
                            Node { node with Son = son }
                    elif node.Char > c then
                        let son = recurse Empty tail
                        Node
                            { Char = c; WordEnd = wordend; Son = son; Bro = tr }
                    else
                        let bro = recurse node.Bro lw
                        Node { node with Bro = bro }
        let lword = [ for c in word -> c ]
        recurse tree lword

    // Display tree : one word per line, in alphabetical order (if tree in said order)
    let displayFold tree =
        let fEmpty acc = ()
        let fNode acc node =
            match node.WordEnd with
            | true -> printfn "%s%c" acc node.Char
            | false -> ()
            sprintf "%s%c" acc node.Char
        fold fEmpty fNode tree ""

    // Not tail recursive
    let displayFoldback tree =
        let fEmpty = []
        let fNode node subacc =
            let newsubacc = subacc |> List.map (fun x -> string node.Char + x)
            if node.WordEnd then
                List.append [string node.Char] newsubacc
            else
                newsubacc
        let fCombine resSon tb =
            List.append resSon tb
        let words = foldback fEmpty fNode fCombine tree
        words |> List.map (printfn "%s")

    let rec getBro tree char =
        match tree with
        | Empty -> Empty
        | Node node ->
            if node.Char = char then tree
            else getBro node.Bro char

    // Determine if a word is present in the dictionnary
    let searchWord tree word =
        let rec searchNext tr wd =
            if String.length wd = 0 then true
            else
                let bro = getBro tr wd.[0]
                match bro with
                | Empty -> false
                | Node node ->
                    if String.length wd = 1 && not node.WordEnd then false
                    else searchNext node.Son wd.[1..]
        searchNext tree word

    // List all possible partitions (of a given word) of type : (one char, all other letters)
    let partitionsOneOthers word =
        [0 .. String.length word - 1]
        |> List.map (fun ind ->
            (word.[ind], word.[0 .. ind - 1] + word.[ind + 1 ..]))
        |> List.groupBy (fun (char, others) -> char)
        |> List.map (fun (key, list) -> list.Head)
        |> List.sortBy (fun (ch, wd) -> ch)
                
    // List anagrams of given word (including those with some letters left aside)
    let rec anagrams tree word =
        let rec recurse hword tword wdlist =
            match tword with
            | "" -> wdlist
            | _ ->
                let subs = partitionsOneOthers tword
                let newlist = 
                    subs
                    |> List.collect (fun (ch, wd) ->
                        let newword = hword + string ch
                        if searchWord tree newword then [newword]
                        else [])
                let reclist =
                    subs |> List.collect (fun (ch, wd) ->
                    recurse (hword + string ch) wd [])
                List.append wdlist (List.append newlist reclist)
        recurse "" word []

    // List anagrams of a given word having a given length
    let anagramsWithLength tree word size =
        let anagrams = anagrams tree word
        anagrams
        |> List.filter (fun wd -> wd.Length = size)

    // List anagrams of a given word with maximum length
    let longestAnagrams tree word =
        let anagrams = anagrams tree word
        anagrams
        |> List.groupBy (fun wd -> wd.Length)
        |> List.sortByDescending (fun (len, wdlist) -> len)
        |> List.head
        |> snd

module FTree =

    type Head = char * bool // Char * WordEnd

    type FTree =
        | Nil
        | Node of head: Head option * sons: FTree array

    let charIndex char = (int char) - (int 'a')

    let initSons () = Array.init 26 (fun i -> Nil)

    let addWord tree (word: string) =
        let rec recurse tr lwd =
            match lwd with
            | [] -> tr
            | c :: tail ->
                match tr with
                | Nil ->
                    let newsons = initSons ()
                    if tail.Length > 0
                    then Array.fill newsons (charIndex tail.Head) 1 (recurse Nil tail)
                    Node(Some(c, tail.IsEmpty), sons = newsons)
                | Node (None, nsons) ->
                    let index = charIndex c
                    Array.fill nsons index 1 (recurse nsons.[index] lwd)
                    tr
                | Node (Some (nchar, nend), nsons) ->
                    match tail with
                    | [] -> Node(Some(nchar, true), nsons)
                    | h :: _ ->
                        let index = charIndex h
                        Array.fill nsons index 1 (recurse nsons.[index] tail)
                        Node(Some(nchar, nend), nsons)

        let lword = Seq.toList word
        match tree with
        | Nil -> recurse (Node(None, initSons ())) lword
        | Node _ -> recurse tree lword

    // For foldback see Thomas Petricek
    // https://stackoverflow.com/questions/16875835/fold-recursion-over-multiway-tree-in-f/16876066#16876066
    let fold fNil fNode fCombine accu tree: 'r =
        let rec recurse acc tr =
            match tr with
            | Nil -> fNil acc
            | Node (header, nsons) ->
                let newacc = fNode acc header
                nsons
                |> Array.map (recurse newacc)
                |> Array.reduce fCombine

        recurse accu tree

    let display tree =
        let fNil acc = acc

        let fNode acc header =
            match header with
            | None -> acc
            | Some (nchar, nend) ->
                let newacc = acc + (string nchar)
                if nend then printfn "%s" newacc
                newacc

        let fCombine sumacc xacc = xacc
        fold fNil fNode fCombine "" tree

    let searchWordSlow tree word =
        let fNil acc = acc

        let fNode acc header =
            match acc with
            | true, _ -> acc
            | false, wd ->
                match header with
                | None -> acc
                | Some (nchar, nend) ->
                    let newwd = wd + (string nchar)
                    ((newwd = word) && nend, newwd)

        let fCombine sumacc xacc =
            let found, formedword = sumacc
            if found then sumacc else xacc

        fold fNil fNode fCombine (false, "") tree

    let searchWord tree word =
        let rec recurse lwd tr =
            match tr with
            | Nil -> lwd
            | Node (nhead, nsons) ->
                match lwd with
                | [] -> lwd
                | c :: tail ->
                    match nhead with
                    | None -> recurse lwd nsons.[charIndex c]
                    | Some (nchar, nend) ->
                        match tail, nend with
                        | [], true -> tail
                        | _ -> recurse tail nsons.[charIndex tail.Head]

        let res = recurse (Seq.toList word) tree
        res.IsEmpty

// Tests
[<EntryPoint>]
let main argv =

    let mySeqDict = Seq.initSeqDict
    let myBinaryDict = Seq.fold BinTree.addWord BinTree.Empty mySeqDict
    // printfn "%b" (BinTree.searchWord myBinaryDict "abais")
    // BinTree.displayFoldback myBinaryDict |> ignore
    let word = "abaissa"
    // let anagrams = BinTree.anagrams myBinaryDict word
    // printfn "%A" anagrams
    let longestana = BinTree.longestAnagrams myBinaryDict word
    printfn "%A" longestana
    0