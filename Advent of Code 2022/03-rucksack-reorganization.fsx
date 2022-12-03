#load "Utils.fsx"
open System
open Utils

let findCommonLetter (bags: seq<string>) =
    let fstBag, restBags = Seq.head bags, Seq.tail bags

    let rec findIt = function        
        | [] -> ' ' // this is not happening
        | x::xs -> 
            if restBags |> Seq.forall (fun bag -> Seq.contains x bag) then
                x
            else
                findIt xs
    
    findIt (Seq.toList fstBag)

let charToScore (c: char) =
    let isUpperChar = c >= 'A' && c <= 'Z'
    let point = int c - int 'a' + 1
    if isUpperChar then
        point + 58
    else
        point

let solveFirst (input : seq<string>) =
    let splitBagInTwo (bag: string): seq<string> =
        let bagListLength = Seq.length bag
    
        seq {
            bag |> Seq.take (bagListLength / 2) ; 
            bag |> Seq.skip (bagListLength / 2) 
        }
        |> Seq.map System.String.Concat

    input
    |> Seq.map splitBagInTwo 
    |> Seq.map findCommonLetter
    |> Seq.map charToScore
    |> Seq.sum


let solveSecond (input: seq<string>) =    
    let threeElfGroups = input |> Seq.chunkBySize 3 |> Seq.map (Array.toSeq)

    threeElfGroups
    |> Seq.map findCommonLetter
    |> Seq.map charToScore
    |> Seq.sum


let input = IO.readLines "03-rucksack-reorganization.txt"

solveFirst input
solveSecond input
