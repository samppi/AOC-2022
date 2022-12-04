#load "Utils.fsx"
open Utils

type AssPair = (int * int) * (int * int)

let intoRangePair (input:seq<string>) =
    let assignmentToTuple (assignment : string) = 
        let res = assignment.Split "-"
        (int res.[0], int res.[1])

    input
    |> Seq.map (fun x -> x.Split ",")
    |> Seq.map (fun x -> x.[0], x.[1])
    |> Seq.map (
        fun (a,b) -> (assignmentToTuple  a, assignmentToTuple b)
    )

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

let countWithCondition condition pairs =
    pairs
    |> Seq.map condition
    |> Seq.filter (fun x -> x)
    |> Seq.length

let input = IO.readLines "04-tbd.txt"
let pairs = intoRangePair input

countWithCondition contains pairs 
countWithCondition overlaps pairs 