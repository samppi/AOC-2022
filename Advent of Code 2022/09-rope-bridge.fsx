open System
#load "Utils.fsx"
open Utils

type Dir = 
    | U
    | R
    | D
    | L
type Pos = (int*int)

let flattenCmd ((c,i): (Dir*int)) = List.init i (fun _ -> c)

let parseInput (row : string) : Dir list = 
    let splitInput = row.Split " "
    let dir =
        match Seq.head splitInput[0] with
        | 'U' -> U
        | 'R' -> R
        | 'D' -> D
        | 'L' -> L
        | _ -> failwith "Invalid direction"

    flattenCmd (dir, int splitInput[1])
    
let moveHead (dir: Dir) (pos: Pos) = 
    let (x,y) = pos
    match dir with
    | U -> (x,y+1)
    | R -> (x+1,y)
    | D -> (x,y-1)
    | L -> (x-1,y)

let sign x =
    if x > 0 then 1
    else if x < 0 then -1
    else 0

let followPrevNode (prev: (int * int)) (cur: (int * int)) =
    let (px, py) = prev
    let (cx, cy) = cur

    let dx, dy = px - cx, py - cy

    // If the head is two steps away from the tail in any direction, move the tail one step in that direction
    if dx = 2 || dx = -2 || dy = 2 || dy = -2 then
        (cx + sign dx, cy + sign dy)
    // No need to move
    else if abs dx = 1 && abs dy = 1 then
        (cx,cy)
    // head and tail aren't touching and aren't in the same row or column, move the tail one step diagonally
    else if dx <> 0 && dy <> 0 then
        (cx + sign dx, cy + sign dy)
    // Otherwise, don't move the tail
    else
        (cx, cy)  

let collectTailPositions (cmds: Dir List) knotCount =
    let initialKnots: Pos list =
        [0..(knotCount - 1)] |> List.map (fun _ -> (0,0))
    
    let rec loop (dirLst: Dir list) (headPos: Pos) (tailKnots: Pos list) (acc: Pos list) = 
        match dirLst with
        | [] -> acc
        | dir::xs ->            
            let newHead = moveHead dir headPos
            let allNodes =
                tailKnots
                |> List.rev
                |> List.fold (
                    fun acc c ->
                        let thisPos = followPrevNode (List.head acc) c
                        List.append [thisPos] acc
                ) [newHead]

            let newTailKnots = List.take (List.length allNodes - 1) allNodes

            loop xs newHead newTailKnots ((List.head newTailKnots)::acc)

    loop cmds (0,0) initialKnots []

let distinctTailPositionCount input tailLength =
    let dirList = input |> Seq.collect parseInput |> Seq.toList
    collectTailPositions dirList tailLength
    |> List.distinct
    |> List.length

let input = IO.readLines "09-rope-bridge.txt"
// Silver: 5907
distinctTailPositionCount input 1
// Gold 2303
distinctTailPositionCount input 9