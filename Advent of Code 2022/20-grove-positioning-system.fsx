#load "Utils.fsx"
open Utils

let findNewIndex (arr: List<int>) i (amount: int64) =
    let newIndex = int ( (i + amount) % int64 (arr.Length - 1))
    if newIndex <= 0 then arr.Length + newIndex - 1
    else newIndex

let mixNumbers (input: int64 array) times =
    let mixed = input |> Array.mapi (fun i x -> (i,x))
    
    let mutable arr = List.init (Array.length input) (fun i -> i)
    for _ in 1..times do
        mixed
        |> Array.iter(
            fun (origIndex, amount) ->
                let curIndex = arr |> List.findIndex (fun x -> x = origIndex)
                let newIndex = findNewIndex arr curIndex amount
            
                let oldItem = arr.[curIndex]
                arr <- arr |> List.removeAt curIndex |> List.insertAt newIndex oldItem
                
        )    
    arr

let solve (intInput: int64 array) (decryptionKey: int64) times = 

    let decrypted = intInput |> Array.map (fun x -> x * decryptionKey)
    let res  = mixNumbers decrypted times

    let getNormzliedIndexd (inp: int64 array) res =
        let oriuginalZi = inp |> Array.findIndex (fun x -> x = 0)
        res |> List.findIndex (fun x -> x = oriuginalZi)
  
    let zi = getNormzliedIndexd  decrypted res

    [1000;2000;3000]
    |> List.map (fun i -> (i + zi) % List.length res)
    |> List.map (fun i -> List.item i res)
    |> List.map (fun i -> decrypted[i])
    |> List.sum

let intInput = IO.readLines "20-grove-positioning-system.txt" |> Seq.map int64 |> Seq.toArray

// First part
solve intInput 1 1
// Second part
solve intInput 811589153L 10