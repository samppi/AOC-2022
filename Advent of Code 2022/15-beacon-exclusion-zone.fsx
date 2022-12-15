#load "Utils.fsx"
open Utils

type Pos = int*int
type SensorData = {
    Sensor: Pos;
    Strength: int;
    Beacon: Pos;
    }
    

let manhattanDistance (x1,y1) (x2,y2) = 
    abs (x1 - x2) + abs (y1 - y2)

let isNumber (s:string) = 
    let isDigit c = c >= '0' && c <= '9'
    let isSign c = c = '-' || c = '+'
    let isDot c = c = '.'
    let isExp c = c = 'e' || c = 'E'
    let isNumber c = isDigit c || isSign c || isDot c || isExp c
    s.Length > 0 && s |> Seq.forall isNumber
    
let parseBeaconData (row:string): SensorData = 
    let numbers = row.Split [|' ';',';'=';':'|] |> Array.filter isNumber
    
    let arr = numbers |> Array.map (fun x -> int x)
    let sensor = (arr.[0], arr.[1])
    let beacon = (arr.[2], arr.[3])
    {Sensor = sensor; Beacon = beacon; Strength = manhattanDistance sensor beacon}

let findReachablePointsWithinY (yLine:int) (d: SensorData) =
    let isWithinReach point =
        manhattanDistance d.Sensor point <= d.Strength    
    let startingPoint = (fst d.Sensor, yLine)
    
    if isWithinReach startingPoint then
        let rec findReachablePoints (xSet: Set<int>) delta = 
            let x1 = fst startingPoint + delta
            
            if isWithinReach (x1, yLine) then
                let x2 = fst startingPoint - delta
                let newSet = [x1; x2] |> List.fold (fun acc x -> Set.add x acc) xSet
                findReachablePoints newSet (delta + 1)
            else
                xSet
        
        findReachablePoints (Set.empty.Add (fst d.Sensor)) 1
    else
        Set.empty

let isBeaconFree (dList: SensorData list) point = 
    dList |> List.exists (fun d -> d.Beacon = point) |> not

let getBeaconLessPointsOnLine dataList yLine withBeacons = 
    let res = 
        dataList
        |> List.map (findReachablePointsWithinY yLine)
        |> List.fold Set.union Set.empty
        |> Set.toList

    if withBeacons then
        res
    else
        res |> List.filter (fun x -> isBeaconFree dataList (x,yLine))
 

let findSensor (dList: SensorData list) (point: int*int) =
    dList |> List.tryFind (fun d -> manhattanDistance point d.Sensor <= d.Strength)
    
let coordRange = (0, 4000000)
let getHoleInLine (dList: SensorData list) (yLine:int) =    
    let rec findHole (x:int) =
        if x > snd coordRange then
            None
        else
            match findSensor dList (x,yLine) with
            | None -> Some(x,yLine)
            | Some(s) ->
                let distance = manhattanDistance s.Sensor (x, yLine)
                let jump = max (s.Strength - distance) 1
                findHole (x + jump)
    
    findHole (fst coordRange)

let findStuff (dList : SensorData list) =
    let rec findStuff' (yLine:int) =
        if yLine > snd coordRange then
            None
        else
            match getHoleInLine dList yLine with
            | None -> findStuff' (yLine+1)
            | Some(x,y) -> Some(x,y)
    
    findStuff' (fst coordRange)

let solveSilver data = 
    let answer = 
        getBeaconLessPointsOnLine data 2000000 false
        |> List.length
    printfn "Silver: %i" answer


let solveGold data = 
    let coord = findStuff data
    match coord with
        | None -> printfn "No hole found"
        | Some(x,y) ->    
            let x64 = int64 x
            let y64 = int64 y    
            
            let answer = x64 * 4000000L + y64
            printfn "Gold: %i" answer

let realSensorDataList =
    IO.readLines "15-tbd.txt" |> Seq.toArray |> Array.map parseBeaconData |> Array.toList

solveSilver realSensorDataList
solveGold realSensorDataList