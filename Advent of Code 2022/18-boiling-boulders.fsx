#load "Utils.fsx"
open Utils

type Cube = int*int*int

let findNeighbourCubes (x,y,z) (droplets: Cube array): Cube list =
    let neighbours = 
        [ (x-1,y,z)
          (x+1,y,z)
          (x,y-1,z)
          (x,y+1,z)
          (x,y,z-1)
          (x,y,z+1) ]

    neighbours |> List.filter (fun x -> Array.contains x droplets)

let getAllNeighbours (x,y,z): Cube list =
    [   (x-1,y,z)
        (x+1,y,z)
        (x,y-1,z)
        (x,y+1,z)
        (x,y,z-1)
        (x,y,z+1) ]
    
let countTheSurfaceArea droplets =
    droplets
        |> Array.map (
            fun cube ->
                findNeighbourCubes cube droplets
                |> List.length
                |> (fun x -> 6 - x)
            )
        |> Array.sum

let findAir (droplets: Cube array) =
    let maxX = droplets |> Array.map (fun (x,_,_) -> x) |> Array.max
    let maxY = droplets |> Array.map (fun (_,y,_) -> y) |> Array.max
    let maxZ = droplets |> Array.map (fun (_,_,z) -> z) |> Array.max
    
    let minX = droplets |> Array.map (fun (x,_,_) -> x) |> Array.min
    let minY = droplets |> Array.map (fun (_,y,_) -> y) |> Array.min
    let minZ = droplets |> Array.map (fun (_,_,z) -> z) |> Array.min
    
    let isOutOfBounds (x,y,z) =        
        x < minX || x > maxX || y < minY || y > maxY || z < minZ || z > maxZ

    let air =
        [minX..maxX]
        |> List.collect  (fun x -> [minY..maxY] |> List.collect (fun y -> [minZ..maxZ] |> List.map (fun z -> (x,y,z))))
        |> List.filter (fun c -> not (Array.contains c droplets))
    
    let mutable pockets = Set.empty<Cube>
    let mutable notPockets = Set.empty<Cube>

    let mutable visited = Set.empty<Cube>

    let rec isWayThrough (cube: Cube) =
        visited <- visited.Add cube
        
        if isOutOfBounds cube then true
        else
            let neighbours = getAllNeighbours cube
            
            if List.exists (fun x -> notPockets.Contains x) neighbours then true
            else
                let newNeighbours =
                    neighbours
                    |> List.filter (fun x -> not (Set.contains x visited))
                    |> List.filter (fun x -> not (Array.contains x droplets))
                    
                newNeighbours |> List.exists isWayThrough
        
    air
    |> List.iter (
        fun cube ->
            if Set.contains cube pockets || Set.contains cube notPockets then ()
            else 
                visited <- Set.empty<Cube>
                if isWayThrough cube then
                    notPockets <- visited |> Set.union notPockets
                else
                    pockets <- visited |> Set.union pockets
    )
    pockets

let input = IO.readLines "18-boiling-boulders.txt" |> Seq.toArray

let droplets: Cube array =
    input 
    |> Array.map(
        fun x ->
            match x.Split "," |> Array.map int with
            | [|x;y;z|] -> (x,y,z)
            | _ -> failwith "Invalid input")
#time
let pockets = findAir droplets

let totalSurface = countTheSurfaceArea droplets
let pocketSurface = countTheSurfaceArea (pockets |> Set.toArray)

printfn "Result: part1: %d part2: %d" totalSurface (totalSurface - pocketSurface)
