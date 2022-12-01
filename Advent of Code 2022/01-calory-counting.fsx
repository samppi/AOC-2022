open System.IO

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

let filename = System.IO.Path.Combine("C:\\hobby\\AOC-2022\\Advent of Code 2022\\input", "01-calory-counting.txt")

let splitIntoIntChunks (inputStr : string) (delimiter : string) = 
    inputStr.Split delimiter 
    |> Seq.map (fun x -> x.Split "\n") 
    |> Seq.map (fun x -> x |> Seq.map int |> Seq.toList) 
    |> Seq.toList
    
let findHighest lst = 
    lst
    |> List.map (List.sum)
    |> List.max

let findTopThreeSum lst = 
    lst
    |> List.map (List.sum)
    |> List.sortDescending
    |> List.take 3
    |> List.sum
    
let input = File.ReadAllText filename
let chunks = splitIntoIntChunks input "\r\n\r\n"

findHighest chunks //silver
findTopThreeSum chunks //gold
    
 // DEV
let caloryList = splitIntoIntChunks rawInput "\n\n"
findHighest caloryList
findTopThreeSum caloryList