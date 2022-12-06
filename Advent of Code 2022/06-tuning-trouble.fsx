#load "Utils.fsx"
open Utils

let customSubString len (str : string) endIndex =
    str[(endIndex - len + 1)..(endIndex)]
    
let findStartOfPacketMarker str markerSize =
    let subStr = customSubString markerSize str

    let rec findIt index =
        match subStr index |> Set.ofSeq |> Set.count = markerSize with
        | true -> index + 1 
        | false -> findIt (index + 1)
    findIt (markerSize - 1)

let input = IO.readFileAsString "06-tuning-trouble.txt"

findStartOfPacketMarker input 4
findStartOfPacketMarker input 14