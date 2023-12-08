open System.IO
open System
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let example = readLines "./example"
let example2 = readLines "./example2"
let example3 = readLines "./example3"
let input = readLines "./input"

let reLR = Regex "[LR]+"
let reNode = Regex "([12A-Z]{3}) = \(([12A-Z]{3}), ([12A-Z]{3})\)"

let parseLR line : seq<char> =
    reLR.Match(line).Groups[0].Value

let parseNode line : string * string * string =
    let m = reNode.Match(line)
    (m.Groups[1].Value, m.Groups[2].Value, m.Groups[3].Value)

let nodeMap = input |> Seq.tail |> Seq.tail |> Seq.map parseNode
                       |> Seq.fold (fun m (n, nl, nr) -> Map.add n (nl,nr) m) Map.empty

let step node lr =
   let (nl, nr) = (Map.tryFind node nodeMap).Value
   if lr = 'L' then nl else nr

let go startNode endWith wList =
    let rec goAux l (node, ctnr) =
        if endWith node then ctnr
        else
            match l with
            | [] -> goAux wList (node, ctnr)
            | lr :: l' ->
                goAux l' (step node lr, ctnr + 1)
    goAux wList (startNode,0)

let result = input |> Seq.head |> parseLR |> Seq.toList |> go "AAA" (fun node -> node = "ZZZ")

let endWith chr (node : string) = node[2] = chr

let navList = input |> Seq.head |> parseLR |> Seq.toList

let rec gcd (a: int64) (b: int64) = if a % b = 0 then b else gcd b (a % b)
let lcm (a: int64) (b: int64) =  a * b / gcd a b

let result2 = [for node in (Map.keys nodeMap) do node]
              |> List.filter (endWith 'A')
              |> List.map (fun startNode -> go startNode (endWith 'Z') navList) |> List.map int64 |> List.reduce lcm
