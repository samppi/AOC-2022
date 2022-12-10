open System
#load "Utils.fsx"
open Utils

let parseRow (row : string) =
    let splitRow = row.Split " "
    match splitRow[0] with
    | "noop" -> 0
    | "addx" -> int splitRow[1]
    | _ -> failwith "unknown order"

type PendingCmd = int*int

let isPixelLit (cycle:int) (value:int) =
    let pixelDrawn  = (cycle - 1) % 40
    
    [value  - 1; value; value + 1]
    |> List.exists (fun x -> x = pixelDrawn)

let processCommands input = 
    let checks = [20;60;100;140;180;220] 

    let mutable crt = List.empty<char>

    let addPixel (cycle:int) (value: int) =
        let nextChar = if isPixelLit cycle value then '#' else '.'
        crt <- nextChar::crt

    let rec processRound cmdList (pending : PendingCmd list) (rndCount:int) (prevValue:int) acc =
        
        let curValue = 
            prevValue + 
            (pending 
            |> List.filter (fun c -> fst c = 0)
            |> List.map snd
            |> List.sum)

        addPixel rndCount curValue

        let newAcc =
            if List.contains  rndCount checks then
                (curValue * rndCount)::acc
            else
                acc

        let stillPending =
            pending
            |> List.filter ( fun c -> fst c > 0)
            |> List.map (fun (x,y) -> (x - 1, y))

        if List.length stillPending > 0 then
            processRound cmdList stillPending (rndCount + 1) prevValue newAcc
        else
            match cmdList with
            | [] -> acc
            | x::xs ->
                let pending = if x = 0 then [(0,0)] else [(1, x)]
                processRound xs pending (rndCount + 1) curValue newAcc

    let lst = processRound input [] 1 1 [] |> List.rev

    (lst, crt)


let input = (IO.readLines >> Seq.toList >> fun x -> List.map parseRow x) "10-cathode-ray-tube.txt"
let result = processCommands input

// Silver
List.sum (fst result)

// Gold
(snd result)
|> List.rev
|> List.chunkBySize 40 
|> List.iter (fun x ->
    x |> List.map string |>String.concat "" |> printfn "%s")
