open System
#load "Utils.fsx"
open Utils

let createTreeGrid inp = 
    let charToInt (c: char) = int c - int '0'
    inp |> Seq.map (Seq.map charToInt) |> Seq.map Seq.toList |> Seq.toList

let isVisibleFromEdge (treeLst : int list) treeIndex =
    if treeIndex = 0 ||  treeIndex + 1 = Seq.length treeLst then
        true
    else
        let treeHeight = List.item treeIndex treeLst
        let treesBefore = List.take treeIndex treeLst
        let treesAfter = List.skip (treeIndex + 1) treeLst

        (List.maxBy id treesBefore) < treeHeight
        ||
        (List.maxBy id treesAfter) < treeHeight

let getTreeColumn (treeGrid : int list list) index = 
    treeGrid |> List.map (List.item index)

let getTreeRow (treeGrid : int list list) index = 
    treeGrid |> List.item index

let isVisible treeGrid rowIndex colIndex =
    isVisibleFromEdge (getTreeRow treeGrid rowIndex) colIndex
    ||
    isVisibleFromEdge (getTreeColumn treeGrid colIndex) rowIndex
  
let getVisibleTrees treeGrid =
    treeGrid
    |> List.mapi
        (fun rowIndex row -> 
         row
         |> List.mapi(fun colIndex _ -> isVisible treeGrid colIndex rowIndex))

let rec countUntil lst limit count = 
    match lst with
    | [] -> count
    | x::xs -> 
        if x < limit then
            countUntil xs limit (count + 1)
        else
            count + 1

let max a b = if a > b then a else b
let min a b = if a < b then a else b

let findVisibleTreesInLine treeList index =
    let upperBound = List.length treeList - 1
    let treeHeight = List.item index treeList 
    
    let treesBefore = List.take (max 0 (index)) treeList
    let treesAfter = List.skip (min (upperBound + 1) (index + 1)) treeList
    
    [countUntil (treesBefore |> List.rev) treeHeight 0;
     countUntil treesAfter treeHeight 0;]


let findVisibleTreeCount treeGrid rowIndex colIndex =    
    List.append 
        (findVisibleTreesInLine  (getTreeRow treeGrid rowIndex) colIndex)
        (findVisibleTreesInLine  (getTreeColumn treeGrid colIndex) rowIndex)

let getScenicScores treeGrid =
    let scenicScore = List.fold (*) 1
    let getScenicScore rowIndex colIndex = 
        scenicScore (findVisibleTreeCount treeGrid rowIndex colIndex)
    
    treeGrid
    |> List.mapi(
        fun rowIndex row ->
            [0..(List.length row - 1)]
            |> List.map(getScenicScore rowIndex))

let treeGrid = (IO.readLines >> createTreeGrid) "08-treetop-tree-house.txt"

// Silver
getVisibleTrees treeGrid
|> List.map (List.filter id >> List.length)
|> List.sum

// Gold
getScenicScores treeGrid
|> List.map List.max
|> List.max
