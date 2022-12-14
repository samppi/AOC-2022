#load "Utils.fsx"
open Utils

type Point = int*int
let origin = (500, 0)

let parseObstacles  = 
    let getConcreteObstacles (lines : string array) =
        let parseLine (line: string) =
            line.Split "->"
            |> Array.map (
                fun s ->
                    let parts = s.Split ","
                    (int parts.[0], int parts.[1]))

        lines |> Array.map parseLine

    let rec getLineCoordinatesBetweenPoints (x1,y1) (x2,y2) =
        if x1 = x2 then
            [for y in min y1 y2 .. max y1 y2 -> (x1,y)]
        elif y1 = y2 then
            [for x in min x1 x2 .. max x1 x2 -> (x,y1)]
        else
            failwith "Not a straight line"

    let createObstacleSet (obstacles: (int*int) array array) =
        obstacles
        |> Array.fold (
            fun acc (obstacle: (int*int) array) ->
                obstacle
                |> Array.pairwise
                |> Array.map (fun (p1,p2) -> getLineCoordinatesBetweenPoints p1 p2)
                |> Array.fold (
                    fun acc line ->                    
                        line
                        |> List.fold (fun acc p -> Set.add p acc) acc
                    )
                    acc
            ) Set.empty<Point>

    (getConcreteObstacles >> createObstacleSet)

let isOccupied obs point = Set.contains point obs

type NewLoc = 
    | Point of Point
    | Abyss
    
let getLandingPoint obstacles dropPoint: NewLoc =    
    let lowestPoint =
        obstacles
        |> Set.toList
        |> List.map snd
        |> List.max
    
    let isAbyss p = snd p > lowestPoint
    let canMoveThrough p = not (isOccupied obstacles p)
        
    let moveDown (x,y): Point = (x,y+1)
    
    let rec dropFrom (curPoint:Point): NewLoc =
        if isAbyss curPoint then
            Abyss
        else
            let nextPoint = moveDown curPoint
            if canMoveThrough nextPoint then
                dropFrom nextPoint
            else
                Point curPoint

    dropFrom dropPoint

let getLandingPointWithFloor obstacles floorY dropPoint: NewLoc =    
    let canMoveThrough p = 
        not (isOccupied obstacles p) &&
        snd p  < floorY
        
    let moveDown (x,y): Point = (x,y+1)
    
    let rec dropFrom (curPoint:Point): NewLoc =
        let nextPoint = moveDown curPoint
        if canMoveThrough nextPoint then
            dropFrom nextPoint
        else
            Point curPoint
        
    dropFrom dropPoint
    
let dropGrainOfSand getLandingPointFn obs dropPoint (floorY: int): NewLoc =
    let tricklesDown obs point =
        if snd point + 1 = floorY then
            None
        else    
            let left = (fst point - 1, snd point + 1)
            let right = (fst point + 1, snd point + 1)
    
            let trickles = 
                [left; right]
                |> List.filter (fun point -> not (isOccupied obs point))
        
            List.tryHead trickles
    
    let rec dropSand' curPoint : NewLoc =
        let newLoc = getLandingPointFn curPoint
        
        match newLoc with
        | Abyss -> Abyss
        | Point p ->
            match tricklesDown obs p with
            | Some (x, y) -> dropSand' (x, y - 1)
            | None -> Point p
            
    dropSand' dropPoint


let rec keepDroppinForSilver obs grainCount =    
    let newLoc = dropGrainOfSand (getLandingPoint obs) obs origin 0
    
    match newLoc with
    | Abyss -> grainCount
    | Point p -> keepDroppinForSilver (Set.add p obs) (grainCount + 1)
    
let rec keepDroppinForGold obs floorY grainCount =
    let newLoc = dropGrainOfSand (getLandingPointWithFloor obs floorY) obs origin floorY
    
    match newLoc with
    | Point (500,0) -> grainCount + 1
    | Point p -> keepDroppinForGold (Set.add p obs) floorY (grainCount + 1)
    | Abyss -> failwith "Hey, this should not happen!!"
    
let input = IO.readLines "14-regolith-reservoir.txt" |> Seq.toArray
let obstacles = parseObstacles input

keepDroppinForSilver obstacles 0

let floorY = (obstacles|> Set.toList |> List.map snd |> List.max) + 2
keepDroppinForGold obstacles floorY 0