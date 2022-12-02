#load "Utils.fsx"
open Utils

type Hand = Rock | Paper | Scissors

let parseHand = function
    | "X" | "A" -> Rock
    | "Y" | "B" -> Paper
    | _ -> Scissors

let splitAndPair (input : seq<string>) =
    input
    |> Seq.map(fun x -> x.Split " ")
    |> Seq.map(fun x -> (parseHand x.[0], parseHand x.[1]))

let getHandScore = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let getWinningScore elfHand playerHand = 
    match elfHand with
    | Rock -> (function | Rock -> 3 | Paper -> 6 | Scissors -> 0) playerHand
    | Paper -> (function | Rock -> 0 | Paper -> 3 | Scissors -> 6) playerHand
    | Scissors -> (function | Rock -> 6 | Paper -> 0 | Scissors -> 3) playerHand
      
let playRound (hand1, hand2) = getWinningScore hand1 hand2 + getHandScore hand2
   
let playTheGame = Seq.map playRound >> Seq.sum

type Strategy = Win | Draw | Lose
let mapHandsByStrategy (elfHand, playerHand) =
    let strategy =
        match playerHand with
        | Rock -> Lose
        | Paper -> Draw
        | Scissors -> Win

    let newPlayerHand = 
        match elfHand with
        | Rock -> (function | Win -> Paper | Draw -> Rock | Lose -> Scissors) strategy
        | Paper -> (function | Win -> Scissors | Draw -> Paper | Lose -> Rock) strategy
        | Scissors -> (function | Win -> Rock | Draw -> Scissors | Lose -> Paper) strategy
    
    (elfHand, newPlayerHand)

let handPairs = (IO.readLines >> splitAndPair) "02-rps.txt"

// 1st part
playTheGame handPairs

// 2nd part
let secondRoundHands = handPairs |> Seq.map mapHandsByStrategy
playTheGame secondRoundHands
