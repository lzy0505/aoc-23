open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () |> Seq.toArray
}

let example = readLines "./example"
let example2 = readLines "./example2"
let input = readLines "./input"

type Coord = int * int
type Path = list<Coord>
type Dir = W | S | N | E
type PMap = array<array<char>>

let findStartAndCorner (m: PMap) : Coord * Coord =
    m |> Array.mapi (fun rowNum row -> row |> Array.tryFindIndex (fun c -> c = 'S')
                                           |> Option.map (fun colNum -> (rowNum, colNum), Array.length row - 1))
    |> Array.filter Option.isSome |> Array.head |> Option.get
    |> (fun (sCoord, colLen) -> sCoord, (Array.length m - 1, colLen)  )

let getCurrCoord (p: Path) = List.head p

let tryGetNextCoord (c: Coord) d (corner : Coord) =
    match d with
    | N -> if fst c = 0 then None else Some (fst c - 1, snd c)
    | S -> if fst c = fst corner then None else Some (fst c + 1, snd c)
    | W -> if snd c = 0 then None else Some (fst c, snd c - 1)
    | E -> if snd c = snd corner then None else Some (fst c, snd c + 1)

let getDir c =
    match c with
     | '|'-> [S;N]
     | '-'-> [W;E]
     | 'F'-> [S;E]
     | 'L'-> [N;E]
     | '7'-> [S;W]
     | 'J'-> [N;W]
     | 'S'-> [W;N;S;E]
     | _ -> []

let getOpDir d =
    match d with
    | W -> E
    | E -> W
    | S -> N
    | N -> S

let isConnected dir nc =
    let isComp c dir = c |> getDir |> List.contains dir
    isComp nc (getOpDir dir)

let tryExtend (p: Path) toDir coCoord (m: PMap) : list<Path*Dir> =
    let getChar co = m[fst co][snd co]
    let cCoord = getCurrCoord p
    let oNCoord = tryGetNextCoord cCoord toDir coCoord
    if oNCoord.IsNone then []
    else
      let nCoord = Option.get oNCoord
      let nChar = getChar nCoord
      if not (nChar = 'S') && List.contains nCoord p then []
      elif isConnected toDir nChar
      then getDir nChar |> List.filter (fun d -> not (d = getOpDir toDir))
                        |> List.map (fun d -> nCoord :: p, d)
      else []

let search (m: PMap) sCoord coCoord =
    let getChar co = m[fst co][snd co]
    let getLoopLen (p, _) = p |> List.length
    let rec searchAux (pList: list<Path*Dir>) =
      let (lList, pList') = pList |> List.map (fun (p, d) -> tryExtend p d coCoord m) |> List.reduce (List.append)
                            |> List.partition (fun (p, _)-> getCurrCoord p |> getChar = 'S')
      match (lList, pList') with
      | [], [] -> None
      | [], _ -> searchAux pList'
      | _, [] -> lList |> List.maxBy getLoopLen |> fst |> Some
      | _, _ ->
          let l1 = lList |> List.maxBy getLoopLen |> fst
          match searchAux pList' with
          | Some l2 -> if List.length l1 > List.length l2 then Some l1 else Some l2
          | _ -> Some l1
    searchAux (getDir 'S' |> List.map (fun d -> [sCoord],d)) |> Option.get

let m = input |> Seq.toArray

let (sCoord,coCoord) = findStartAndCorner m

let loop' = search m sCoord coCoord

let result1 = loop' |> List.length |> (fun l -> l / 2)

let loop = List.tail loop'

let _ =
    let sc = loop[List.length loop - 1]
    let sc1 = List.head loop
    let sc2 = loop[List.length loop - 2]
    let computeD c2 =
        match fst sc - fst c2, snd sc - snd c2 with
        | 0, 1 -> W
        | 0, -1 -> E
        | 1, 0 -> N
        | _ -> S
    let d1 = computeD sc1
    let d2 = computeD sc2
    let newS = ['L';'J';'F';'7'] |> List.filter (fun c -> let dl = getDir c
                                                          dl = [d1;d2] || dl = [d2;d1]) |> List.head
    m[fst sc][snd sc] <- newS

let count coord d =
    let getChar co = m[fst co][snd co]
    let isTurn c = (c = 'F' || c = 'L' || c = 'J' || c = '7')
    let isS c1 c2 =
        let b = (not (c1 = c2) && getDir c1 |> List.map getOpDir = getDir c2)
        printfn "S? %s %s $ %b" (string c1) (string c2) b
        b
    let isBound co =
        let ch = getChar co
        let b = match d with
                | N -> (snd co = snd coord) && (fst co < fst coord) && (ch = '-' || isTurn ch)
                | S -> (snd co = snd coord) && (fst co > fst coord) && (ch = '-' || isTurn ch)
                | W -> (fst co = fst coord) && (snd co < snd coord) && (ch = '|' || isTurn ch)
                | E -> (fst co = fst coord) && (snd co > snd coord) && (ch = '|' || isTurn ch)
        if b then Some ch else None
    loop |> List.map isBound |> List.fold (fun (opc, sum) oc ->
                                           match oc with
                                           | None -> (opc,sum)
                                           | Some cc ->
                                             match opc with
                                             | None -> if isTurn cc then (Some cc, sum + 1) else (None, sum + 1)
                                             | Some pc -> if isS pc cc then (None, sum) else (None, sum + 1)) (None, 0) |> snd

let isInside coord =
    let b = if List.contains coord loop then false else [W;E;S;N] |> List.map (fun d -> count coord d % 2 = 1) |> List.forall id
    if b then printfn "(%d,%d) %b" (fst coord) (snd coord) b
    b

let result2 = List.init (fst coCoord + 1) (fun row ->
                                               List.init (snd coCoord + 1) (fun col -> isInside (row,col))
                                                                    |> List.filter id |> List.length
                                                                    ) |> List.sum
