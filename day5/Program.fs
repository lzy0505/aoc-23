open System.IO
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let example = readLines "./example"
let input = readLines "./input"

let reSeed = Regex "(?:seeds):(?: (?<num1>[0-9]+))+"
let reNum = Regex "(?:[a-z]+-to-[a-z]+ map):\r?\n(?:(?<num1>[0-9]+) (?<num2>[0-9]+) (?<num3>[0-9]+)[\r\n]*)+"

let parseSeed line : list<int64> =
    let m = reSeed.Match(line)
    [ for c in m.Groups[1].Captures do yield int64 c.Value]

let parseMaps line : list<list<int64 * int64 * int64>> =
    let ms = reNum.Matches(line)
    [ for m in ms do
        List.zip3
          [for c in  m.Groups[1].Captures do yield int64 c.Value]
          [for c in  m.Groups[2].Captures do yield int64 c.Value]
          [for c in  m.Groups[3].Captures do yield int64 c.Value]
      ]

let genMap (aList: list<int64 * int64 * int64>) : int64 -> int64 =
      fun n -> (n,false)
               |> (aList |> List.map (fun (d,s,r) ->
                                        fun (n, b) -> if (not b) && s <= n && n < s + r then (d + (n - s), true) else (n, b))
                         |> List.reduce (>>))
               |> fst

type Intv = int64 * int64

let mapIntv : int64 -> int64 -> int64 -> (Intv * bool) -> list<Intv*bool> = fun d s r ((ss,sr),b) ->
        if b then [(ss,sr),b]
        else if ss+sr <= s || s+r <= ss then [(ss,sr),false]
             elif s <= ss && ss < s+r then if ss+sr <= s+r then [((d+(ss-s),sr),true)] else [((d+(ss-s),s+r-ss),true);((s+r,(sr-(s+r-ss))),false)]
             elif ss+sr <= s+r then [((ss,s-ss),false);((d,(sr-(s-ss))),true)] else [((ss,s-ss),false);((d,r),true);((s+r,ss+sr-(s+r)),false)]

let genMap2 (aList: list<int64 * int64 * int64>) : list<Intv> -> list<Intv> =
      fun iList ->
          iList |> List.map (fun p -> p, false)
                |> (aList |> List.map (fun (d,s,r) -> List.map (mapIntv d s r) >> List.reduce List.append)
                          |> List.reduce (>>))
                |> List.map fst

let maps = input |> Seq.tail |> String.concat "\r\n" |> parseMaps |> List.map genMap |> List.reduce (>>)
let result1 = input |> Seq.head |> parseSeed |> List.map maps |> List.min

let maps2 = input |> Seq.tail |> String.concat "\r\n" |> parseMaps |> List.map genMap2 |> List.reduce (>>)
let result2 = input |> Seq.head |> parseSeed |> List.chunkBySize 2 |> List.map (fun l -> ((l.Head, l.Tail.Head): Intv)) |> maps2 |> List.map fst |> List.min
