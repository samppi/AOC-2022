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
 
    
module Matrix =
    // get matrix size
    let getSize (matrix: seq<seq<'T>>) =
        let rows = matrix |> Seq.length
        let cols = matrix |> Seq.head |> Seq.length
        (rows, cols)

    // get matrix element
    let getElement (matrix: seq<seq<'T>>) (row: int) (col: int) =
        matrix |> Seq.item row |> Seq.item col

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
    
    
