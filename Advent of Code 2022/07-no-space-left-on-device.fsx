open System
#load "Utils.fsx"
open Utils

type File = {
    loc: string list
    size: int
    name: string
}

let parseFiles input =
    let curDir = [|"/"|] |> Array.toList
    
    let cd (cmd : string) (curDir : string list) =
        let splitCmd =  cmd.Split " "
        if splitCmd.[2] = ".." then            
            List.take (curDir.Length - 1) curDir
        elif splitCmd.[2] = "/" then
            List.empty
        else
            List.append curDir [splitCmd.[2]]

    let rec traverse (lines : string list) (curDir : string list) (fileList: File list) =
        match lines with
        | [] -> fileList
        | x::xs -> 
            if x.StartsWith "$ cd" then
                traverse xs (cd x curDir) fileList
            elif Char.IsDigit x.[0] then
                let fileParts = x.Split " "
                let file: File = {
                    loc = curDir;
                    name = fileParts.[1];
                    size = int fileParts.[0];
                }
                traverse xs curDir (List.append fileList [file])        
            else
                traverse xs curDir fileList

    traverse input curDir  []

let getUniqueFolder (files : File list) = 
    let getAllFoldersAlongTheWay (lst: string list) =
        [1..(List.length lst)] 
        |> List.map (fun x -> List.take x lst)

    files
    |> List.collect (fun f -> (getAllFoldersAlongTheWay f.loc))
    |> Set.ofList
    |> Set.toList

let findFolderSize (fileStructure : File list) (folder : string list) = 
    let isWithinFolder (file: File) = 
        if file.loc.Length < folder.Length then
            false
        else
            List.zip folder (List.take (List.length folder) file.loc)
            |> List.forall (fun (a,b)  -> a = b)

    fileStructure
    |> List.filter isWithinFolder
    |> List.map (fun f -> f.size)
    |> List.sum


let solveFirst input = 
    let files = parseFiles input

    getUniqueFolder files
    |> List.map (findFolderSize files)
    |> List.filter (fun size -> size <= 100000)
    |> List.sum


let solveSecond input =
    let files = parseFiles input
    
    let totalSize = 70000000
    let usedSpace = findFolderSize files []
    let spaceNeeded = 30000000
    let needToFree = spaceNeeded - (totalSize - usedSpace)

    getUniqueFolder files
    |> List.map (findFolderSize files)
    |> List.filter (fun size -> size > needToFree)
    |> List.sort
    |> List.head
    

let input = IO.readLines "07-no-space-left-on-device.txt" |> Seq.toList
solveFirst input
solveSecond input