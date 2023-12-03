open System
open System.IO

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let example = readLines "./example"
let input = readLines "./input"

let collectSymbolCoord isSym mat =
    mat |> Seq.mapi (fun rowNum rSeq -> rSeq |> Seq.mapi (fun curColIdx ch -> if isSym ch then Some (rowNum, curColIdx) else None)
                                             |> Seq.filter Option.isSome |> Seq.map Option.get |> Seq.toList)
        |> List.concat

let isAdjacentMap isSym mat startIdx endIdx rowNum =
    let symbolList = collectSymbolCoord isSym mat
    let totalRowNum = Seq.length mat
    let uBound = if rowNum = 0 then 0 else rowNum - 1
    let lBound = if startIdx = 0 then 0 else startIdx - 1
    let rBound = endIdx
    let dBound = if rowNum = totalRowNum - 1 then rowNum else rowNum + 1
    symbolList |> List.filter (fun (curRowIdx, curColIdx) -> lBound<=curColIdx && curColIdx <= rBound && uBound<=curRowIdx && curRowIdx <=dBound)

type Res = list<int>
type Map = Map<int*int,Res>
type St2 = option<int*string>*Map

let addList aMap key aList =
    match Map.tryFind key aMap with
    None -> Map.add key aList aMap
    | Some l -> Map.add key (List.append l aList) aMap

let mergeListMap aMap bMap =
    aMap |> Map.fold (fun cMap key aList -> addList cMap key aList) bMap

let solve isSym mat =
   let checkAndAdd startIdx endIdx rowNum st aMap =
       let collectedList = isAdjacentMap isSym mat startIdx endIdx rowNum
       // if not (List.isEmpty collectedList) then printfn "num: %s, row: %d" st rowNum
       List.fold (fun m c -> addList m c [(int st)]) aMap collectedList
   let folder : _ -> _ -> St2 -> _ -> St2 =
       fun len row (startFrom, aMap) (curIdx, ch) ->
       match startFrom with
       None -> if Char.IsDigit ch then (Some (curIdx, string ch), aMap) else (None, aMap)
       | Some (startFromIdx, curStr) ->
                       if Char.IsDigit ch then
                           let newStr = curStr + (string ch)
                           if curIdx = len - 1
                           then None, (checkAndAdd startFromIdx curIdx row newStr aMap)
                           else Some (startFromIdx, newStr), aMap
                       else None, (checkAndAdd startFromIdx curIdx row curStr aMap)
   mat |> Seq.mapi (fun rowNum rowSeq -> rowSeq |> Seq.indexed |> Seq.fold (folder (Seq.length rowSeq) rowNum) (None, Map.empty)|>snd)
   |> Seq.reduce mergeListMap


let result1 = solve (fun ch -> (not (Char.IsDigit ch)) && not (ch = '.')) input
              |> Map.map (fun _ aList -> aList |> List.sum)
              |> Map.values |> Seq.reduce (+)
let result2 = solve (fun ch -> ch = '*') input
              |> Map.map (fun _ aList -> if (aList |> List.length) = 2 then aList.Head * (List.tail aList).Head else 0)
              |> Map.values |> Seq.reduce (+)
