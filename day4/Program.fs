open System.IO
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let example = readLines "./example"
let input = readLines "./input"

let re = Regex "(?:(?<num>[0-9 ][0-9]) )+\u007c (?<num2>[0-9 ][0-9][ ]?)+"

let parseLine line : list<int>*list<int> =
    let ms = re.Matches(line)
    [ for c in ms[1].Groups[2].Captures do yield int c.Value], [ for c in ms[2].Groups[2].Captures do yield int c.Value]

let getPoints n = if n = 0 then 0 else pown 2 (n-1)

let solve1 (winList, haveList) =
    haveList |> List.filter (fun num -> winList |> List.exists (fun num' -> num' = num)) |> List.length

let getNewList aList num inc =
    if List.length aList < num then
        aList |> List.map (fun n -> n+inc) |> (fun l -> List.append l (List.init (num - List.length aList) (fun _ -> inc)))
    else
        aList |> List.mapi (fun counter n -> if counter < num then n+inc else n)

let result1 = input |> Seq.map (parseLine >> solve1 >> getPoints) |> Seq.sum
let result2 = input |> Seq.map (parseLine >> solve1)
                    |> Seq.mapFold (fun st points ->
                                    let st1 = getNewList st 1 1
                                    let copies = st1.Head
                                    let newcard = points
                                    let newList = getNewList st1.Tail newcard copies
                                    (copies, newList)
                                    ) [] |> fst |> Seq.sum
