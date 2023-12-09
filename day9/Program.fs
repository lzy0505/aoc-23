open System.IO
open System
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let example = readLines "./example"
let input = readLines "./input"

let re = Regex "([-]?[0-9]+(:= )?)+"

let parse line : list<int> =
    [ for m in re.Matches(line) do yield int m.Value ]

let getDiffList (aList: list<int>) =
    aList |> List.truncate (aList.Length - 1) |> List.zip (aList.Tail) |> List.map (fun (n1, n2) -> n1 - n2)

let go aList =
    let rec goAux dList lSum =
        if List.forall (fun n -> n = 0) dList then lSum
        else goAux (getDiffList dList) (lSum + List.last dList)
    goAux aList 0

let result = input |> Seq.map parse |> Seq.map go |> Seq.reduce (+)

let go2 aList =
    let rec goAux dList fList =
        if List.forall (fun n -> n = 0) dList
        then
            List.fold (fun st n -> n - st) 0 fList
        else goAux (getDiffList dList) (List.head dList :: fList)
    goAux aList []

let result2 = input |> Seq.map parse |> Seq.map go2 |> Seq.reduce (+)
