#load "Utils.fsx"
open Utils

let findNewIndex (lst: int array) i (moveBy: int64) =
    let newIndex = int ( (i + moveBy) % int64 (lst.Length - 1))
    if newIndex <= 0 then lst.Length + newIndex - 1
    else newIndex

let mixNumbers (input: int64 array) times =
    let mixed = input |> Array.mapi (fun i x -> (i,x))
    
    let mutable arr = Array.init (Array.length input) (fun i -> i)
    for _ in 1..times do
        mixed
        |> Array.iter(
            fun (origIndex, amount) ->
                let curIndex = arr |> Array.findIndex (fun x -> x = origIndex)
                let newIndex = findNewIndex arr curIndex amount
            
                let oldItem = arr.[curIndex]
                arr <- arr |> Array.removeAt curIndex |> Array.insertAt newIndex oldItem
                
        )    
    arr

let solve (intInput: int64 array) (decryptionKey: int64) times =
    let decrypted = intInput |> Array.map (fun x -> x * decryptionKey)
    let res  = mixNumbers decrypted times
    
    let origZero = intInput |> Array.findIndex (fun x -> x = 0)        
    let zeroIndex = res |> Array.findIndex (fun x -> x = origZero)
    
    [1000;2000;3000]
    |> List.map (fun i -> (i + zeroIndex) % Array.length res)
    |> List.map (fun i -> Array.item i res)
    |> List.map (fun i -> decrypted[i])
    |> List.sum

let intInput = IO.readLines "20-grove-positioning-system.txt" |> Seq.map int64 |> Seq.toArray

// First part
solve intInput 1 1

// Second part
solve intInput 811589153L 10