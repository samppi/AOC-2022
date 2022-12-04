#load "Utils.fsx"
open Utils

type AssPair = (int * int) * (int * int)

let intoRangePair (input:seq<string>) =
    let toTuple (assignment : string) = 
        let res = assignment.Split "-"
        (int res.[0], int res.[1])

    input
    |> Seq.map (fun x -> x.Split ",")
    |> Seq.map (fun x -> toTuple x.[0],toTuple x.[1])

let isWithin nro (rangeStart,rangeEnd) = nro >= rangeStart && nro <= rangeEnd

let contains ((fstPair, sndPair) : AssPair) =
    isWithin (fst fstPair) sndPair && isWithin (snd fstPair) sndPair
    ||
    isWithin (fst sndPair) fstPair && isWithin (snd sndPair) fstPair

let overlaps ((fstPair, sndPair) : AssPair) =
    isWithin (fst fstPair) sndPair || 
    isWithin (snd fstPair) sndPair || 
    isWithin (fst sndPair) fstPair || 
    isWithin (snd sndPair) fstPair

let countWith condition pairs =
    pairs
    |> Seq.map condition
    |> Seq.filter id
    |> Seq.length

let pairs = (IO.readLines >> intoRangePair) "04-camp-cleanup.txt"

countWith contains pairs 
countWith overlaps pairs 