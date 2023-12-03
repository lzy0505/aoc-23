open System.IO
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let example = readLines "./example"
let input = readLines "./input"

let re = Regex "(?:(?<num>[0-9]+) (?<color>blue|red|green)(?:, )?)+(?=; )?"

let parseLine line : list<list<int*string>> =
    [ for mc in re.Matches(line) do
         yield List.zip [ for c in mc.Groups[1].Captures do yield int c.Value]
                        [ for c in mc.Groups[2].Captures do yield c.Value] ]

let getLimit color = if color = "blue" then 14 elif color = "red" then 12 else 13


let result1 = input |> Seq.map parseLine
                    |> Seq.indexed
                    |> Seq.filter (snd >> List.forall (List.forall (fun (num,color) -> getLimit color >= num)))
                    |> Seq.map (fst >> (fun i -> i+1))
                    |> Seq.sum

let getPower = List.map (snd >> (List.maxBy fst) >> fst) >> List.reduce (*)

let result2 = input |> Seq.map parseLine
                    |> Seq.map (List.toSeq >> List.concat >> List.groupBy snd >> getPower)
                    |> Seq.sum
