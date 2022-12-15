module Utils =
    let trimSplit (delimiter:string) (str:string) = 
            str.Split delimiter
            |> Seq.map (fun x -> x.Trim())
            |> Seq.filter (fun x -> x <> "")

    let charToInt (c: char) = int c - int '0'

    let filterSomes (seq: option<'T> list): 'T list =
        seq
        |> List.fold 
            (fun acc x -> match x with
                            |None -> acc
                            |Some(value) -> value :: acc
                        ) []

    // String seq as int seq
    let asIntSeq (strSeq : seq<string>) =
        strSeq |> Seq.map int

    // string matrix as int matrix
    let asIntMatrix (strMatrix : seq<seq<string>>) =
        strMatrix |> Seq.map asIntSeq

    // Print sequence of strings
    let printSeq (seq: seq<string>) =
        seq
        |> Seq.iter (fun x -> printfn "%s" x)
    let rec last items =
        match items with
        | [] -> failwith "Empty list"
        | [x] -> x
        | x::xs -> last xs

    let trim (x : string) =
        x.Trim()
    
module Matrix =
    // get matrix size
    let getSize (matrix: 'T list list) =
        let rows = matrix |> List.length
        let cols = matrix |> List.head |> List.length
        (rows, cols)

    // get matrix element
    let getElement (matrix: 'T list list) ((row,col): (int*int))  =
        matrix |> List.item row |> List.item col
    
    // get adjacent neighbours
    let getAdjacentNeighbours (matrix: 'T list list) ((row,col): int*int) =
        let rows, cols = getSize matrix
        let neighbours = [
            (row - 1, col - 1); (row - 1, col); (row - 1, col + 1);
            (row, col - 1); (row, col + 1);
            (row + 1, col - 1); (row + 1, col); (row + 1, col + 1)
        ]
        neighbours
        |> List.filter (fun (r, c) -> r >= 0 && r < rows && c >= 0 && c < cols)
    
    let getDirectNeighbours (matrix: 'T list list) ((row,col): int*int) =
        let rows, cols = getSize matrix
        let neighbours = [
            (row - 1, col);
            (row, col - 1); (row, col + 1);
            (row + 1, col);
        ]
        neighbours
        |> List.filter (fun (r, c) -> r >= 0 && r < rows && c >= 0 && c < cols)

open System.IO

let getInputPath filename = 
    Path.Combine("C:\\hobby\\AOC-2022\\Advent of Code 2022\\input", filename)
    

module IO = 
    let readFileAsString (filename: string) =
        File.ReadAllText(getInputPath filename)

    let readLines (filename:string) = seq {
        let filePath = getInputPath filename
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
    
    
