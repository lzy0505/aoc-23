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

let re = Regex "(?<card>[ATJQK2-9]{5}) (?<num>[0-9]{1,4})"

[<StructuralEquality; CustomComparison>]
type Hand = HandC of list<int>
with
    member this.Value = match this with HandC x -> x
    interface IComparable<Hand> with
      member this.CompareTo(h2: Hand) =
        let compareCard = (fun c1 c2 -> if c1 > c2 then 1 elif c1 = c2 then 0 else -1)
        h2.Value |> List.zip this.Value |> List.fold (fun state (c1,c2) -> if state = 0 then compareCard c1 c2 else state) 0
    interface IComparable with
      member this.CompareTo obj =
          match obj with
            | :? Hand as other -> (this :> IComparable<_>).CompareTo other
            | _                    -> -1

let parse line : Hand * int =
    let chrToNum chr = if Char.IsDigit(chr) then int chr - int '0'
                       elif chr = 'T' then 10
                       elif chr = 'J' then 11
                       elif chr = 'Q' then 12
                       elif chr = 'K' then 13
                       else 14
    let m = re.Match(line)
    m.Groups[1].Value |> Seq.toList |> List.map chrToNum |> HandC , int m.Groups[2].Value

[<StructuralEquality; CustomComparison>]
type Kind = Five of Hand | Four of Hand | FullHouse of Hand | Three of Hand | TwoPair of Hand | OnePair of Hand | High of Hand
with
    member this.Hand = match this with Five x -> x | Four x -> x | FullHouse x -> x | Three x -> x |TwoPair x -> x | OnePair x -> x | High x -> x
    interface IComparable<Kind> with
      member this.CompareTo(k2: Kind) =
        match this with
        | Five h1 -> match k2 with
                     | Five h2 -> compare h1 h2
                     | _ -> 1
        | Four h1 -> match k2 with
                     | Five _ -> -1
                     | Four h2 -> compare h1 h2
                     | _ -> 1
        | FullHouse h1 -> match k2 with
                          | Five _ | Four _ -> -1
                          | FullHouse h2 -> compare h1 h2
                          | _ -> 1
        | Three h1 -> match k2 with
                      | Five _ | Four _ | FullHouse _ -> -1
                      | Three h2 -> compare h1 h2
                      | _ -> 1
        | TwoPair h1 -> match k2 with
                        | Five _ | Four _ | FullHouse _ | Three _ -> -1
                        | TwoPair h2 -> compare h1 h2
                        | _ -> 1
        | OnePair h1 -> match k2 with
                        | Five _ | Four _ | FullHouse _ | Three _ | TwoPair _ -> -1
                        | OnePair h2 -> compare h1 h2
                        | _ -> 1
        | High h1 -> match k2 with
                        | High h2 -> compare h1 h2
                        | _ -> -1
    interface IComparable with
      member this.CompareTo obj =
          match obj with
            | :? Kind as other -> (this :> IComparable<_>).CompareTo other
            | _                    -> -1

let parseKind (h : Hand) : Kind =
   let gList = h.Value |> List.groupBy id
   match gList.Length with
   | 1 -> Five h
   | 2 -> if List.exists (fun (_, g: list<int>) -> g.Length = 4) gList then Four h else FullHouse h
   | 3 -> if List.exists (fun (_, g: list<int>) -> g.Length = 3) gList then Three h else TwoPair h
   | 4 -> OnePair h
   | _ -> High h

let result1 = input |> Seq.map parse |> Seq.map (fun (h,p) -> parseKind h, p) |> Seq.sort
                      |> Seq.mapi (fun r (_, p) -> (r + 1) * p) |> Seq.sum

let parse2 line : Hand * int =
    let chrToNum chr = if Char.IsDigit(chr) then int chr - int '0'
                       elif chr = 'T' then 10
                       elif chr = 'J' then 1
                       elif chr = 'Q' then 12
                       elif chr = 'K' then 13
                       else 14
    let m = re.Match(line)
    m.Groups[1].Value |> Seq.toList |> List.map chrToNum |> HandC , int m.Groups[2].Value

let parseKind2 (h : Hand) : Kind =
   let gListWOJoker = h.Value |> List.filter (fun c -> c > 1) |> List.groupBy id
   let useJockerAs = let sl = gListWOJoker |> List.sortByDescending (fun (k, l) -> l.Length, k)
                     if sl.IsEmpty then 1 else sl |> List.head |> fst
   let gList : list<_> = h.Value |> List.map (fun c -> if c = 1 then useJockerAs else c)|> List.groupBy id
   match gList.Length with
   | 1 -> Five h
   | 2 -> if List.exists (fun (_, g: list<int>) -> g.Length = 4) gList then Four h else FullHouse h
   | 3 -> if List.exists (fun (_, g: list<int>) -> g.Length = 3) gList then Three h else TwoPair h
   | 4 -> OnePair h
   | _ -> High h

let result2 = input |> Seq.map parse2 |> Seq.map (fun (h,p) -> parseKind2 h, p) |> Seq.sort
                      |> Seq.mapi (fun r (_, p) -> (r + 1) * p) |> Seq.sum
