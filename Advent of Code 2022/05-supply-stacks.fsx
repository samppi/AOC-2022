#load "Utils.fsx"
open Utils

type Order = int * int * int
type Stacks = string list array
type Instructions =
    { Stacks: Stacks;    Orders: seq<Order> }

let offset = 4
let stackIndexes row =    
    let rowCount = (Seq.length row + 1) / offset
    [0 .. (rowCount - 1)]

let containerAtCol x (row : string) = 
    row.[offset * x + 1].ToString().Trim()

let parseContainerRow (stacks: Stacks) (row : string) =
    stackIndexes row    
    |> List.fold
        (fun (acc : Stacks) x ->
            let container = containerAtCol x row
            if container = "" then
                acc
            else
                let aha = List.append acc[x] [container]
                acc[x] <- aha 
                acc
            ) stacks

let parseOrder (row: string) : Order =
    let splitRow = row.Split " "
    (int splitRow.[1],int splitRow.[3],int splitRow[5])

let parseInput (inputRows: seq<string>) =
    let stackCount = ((Seq.head >> Seq.length) inputRows + 1) / offset

    inputRows
    |> Seq.filter (fun x -> x.Length > 0)
    |> Seq.fold 
        (fun acc row ->
            if row.Contains "[" then
                { acc with Stacks = parseContainerRow acc.Stacks row } : Instructions
            elif Seq.item 0 row = 'm' then
                { acc with Orders = Seq.append acc.Orders [parseOrder row]} : Instructions
            else
                acc
        )
        { Stacks = Array.create stackCount []; Orders = Seq.empty }



let execOrder (stacks : Stacks) ((amount, fromIndex, toIndex): Order) newVersion =
    let moving =
        match newVersion with
        | true -> List.take amount stacks[fromIndex]
        | false -> List.rev (List.take amount stacks[fromIndex])

    stacks[fromIndex] <- List.skip amount stacks[fromIndex]
    stacks[toIndex] <- List.append (List.rev moving) stacks[toIndex]
    stacks

let solve i input =
    let instructions = parseInput input

    instructions.Orders
    |> Seq.map (fun (a,b,c) -> (a,b-1,c-1))
    |> Seq.fold (fun acc order -> execOrder acc order (i = 2)) instructions.Stacks
    |> Seq.map List.head
    |> Seq.toList    


let input = IO.readLines "05-supply-stacks.txt"

// Silver
solve 1 input
// Gold
solve 2 input