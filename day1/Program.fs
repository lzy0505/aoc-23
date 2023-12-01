open System

let readLines (filePath:string) = seq {
    use sr = new IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let example = readLines "./example"
let input = readLines "./input"

let getNumber1 (st: string) =
    let cList = Seq.toList st
    let iList = cList |> List.filter Char.IsDigit
    let toInt : char -> int = fun c -> int c - int '0'
    let (f,l) = (iList |> List.head |> toInt, iList |> List.last |> toInt)
    printfn "%d, %d" f l
    (10 * f + l)

let isSubStart subList aList =
    let subLen = (List.length subList)
    if List.length aList < subLen then false
    else aList |> List.take subLen |> List.forall2 (fun x y -> x = y) subList

let genTruncatedLists aList =
    aList |> List.mapi (fun idx _ -> List.splitAt idx aList |> snd)

let dict = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
            |> List.map Seq.toList

let wordToDigit (st: string) =
    st |> Seq.toList |> genTruncatedLists |> List.map (fun tList ->
                                            let firstChr = tList.Head
                                            if Char.IsDigit firstChr then string firstChr
                                            else
                                               let tmpList = dict |> List.mapi (fun idx kw ->
                                                       if isSubStart kw tList then idx + 1 else 0) |> List.filter (fun idx -> idx > 0)
                                               if tmpList.IsEmpty then "" else string tmpList.Head
                                            ) |> String.concat ""

let result1 = input |> Seq.map getNumber1 |> Seq.reduce (fun x y -> x + y)
let result2 = input |> Seq.map (fun st -> st |> wordToDigit |> getNumber1) |> Seq.reduce (fun x y -> x + y)
