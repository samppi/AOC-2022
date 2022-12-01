#load "Utils.fsx"
open Utils
open IO

let rawInput = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"

let splitIntoIntChunks (inputStr:string) (delimiter: string) = 
    inputStr.Split delimiter 
    |> Seq.map (fun x -> x.Split "\n") 
    |> Seq.map (fun x -> x |> Seq.map int |> Seq.toList) 
    |> Seq.toList

    
let findHighest (lst: int list list) = 
    let calorySums =
        lst
        |> List.map (List.sum)
    
    calorySums |> List.max

let findTopThreeSum (lst: int list list) = 
    let calorySums =
        lst
        |> List.map (List.sum)
    
    calorySums 
        |> List.sortDescending
        |> List.take 3
        |> List.sum


 // DEV
let caloryList = splitIntoIntChunks rawInput "\n\n"
findHighest caloryList
findTopThreeSum caloryList


let input = IO.readFileAsString "01-calory-counting.txt"
let chunks = splitIntoIntChunks input "\r\n\r\n"

// 1st part
findHighest chunks
// 2nd part
findTopThreeSum chunks