open System.IO
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let example = readLines "./example"
let input = readLines "./input"

let re = Regex ":(?:[ ]+(?<num>[0-9]+))+"

let parse line : list<int> =
    let m = re.Match(line)
    [ for c in m.Groups[1].Captures do yield int c.Value]

let races lists =
    List.zip (Seq.head lists) (lists |> Seq.tail |> Seq.head)

let enumerate (time, record) =
     List.init (time+1) (fun i -> i)
     |> List.map (fun pressTime -> (time - pressTime) * pressTime)
     |> List.filter (fun dist -> dist > record)
     |> List.length

let result1 = input |> Seq.map parse |> races |> List.map enumerate |> List.reduce (*)

let oneRace seq =
    seq |> Seq.map (List.map (fun n -> string n) >> List.toSeq >> String.concat "" >> uint64)
    |> (fun s -> Seq.head s, s|> Seq.tail |> Seq.head)

let calc (time, record) =
    let dist pressTime = (time - pressTime) * pressTime
    let rec search from until =
        let middle = (from + until) / 2UL
        let dist = dist middle
        if middle = from then until
        elif dist > record then search from middle
        elif dist < record then search middle until
        else middle
    let zp = search 0UL (time / 2UL)
    ((time / 2UL) - zp) * 2UL + 1UL

let result2 = input |> Seq.map parse |> oneRace |> calc
