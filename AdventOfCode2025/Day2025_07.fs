module Day2025_07

open System.Collections.Generic
open Checked
open FParsec

let parse (s: string) = runLineParser (many (pchar '.' <|> pchar 'S' <|> pchar '^')) s

let printTree (grid: char array array) =
    grid
    |> Seq.map (fun row -> row |> Seq.toArray |> System.String)
    |> Seq.iter (printfn "%s")

let part1 (input: string) =
    let data = parse input |> Seq.map (fun row -> Seq.toArray row) |> Seq.toArray

    let mutable splits = 0
    for i in 1 .. data.Length - 1 do
        for j in 0 .. data.[i].Length - 1 do
            if data.[i - 1].[j] = 'S' then
                data.[i].[j] <- '|'
            elif data.[i - 1].[j] = '|' && data.[i].[j] = '.' then
                data.[i].[j] <- '|'
            elif data.[i - 1].[j] = '|' && data.[i].[j] = '^' then
                splits <- splits + 1
                if data.[i].[j - 1] = '.' then
                    data.[i].[j - 1] <- '|'
                if data.[i].[j + 1] = '.' then
                    data.[i].[j + 1] <- '|'
    
    splits
    //1533

let splitDict = Dictionary<(int * int), int64>()

// This failed due to overflow not being checked. Worked perfectly when changed to int64...
let rec splitCount (data: char array array) (i: int) (j: int): int64 =
    if not (splitDict.ContainsKey((i, j))) then
        if i >= data.Length then
            splitDict.Add((i, j), 1)
        elif data.[i].[j] = '^' then
            splitDict.Add((i, j), splitCount data (i+1) (j-1) + splitCount data (i+1) (j+1))
        elif data.[i].[j] = '.' then
            splitDict.Add((i, j), splitCount data (i+1) (j))
    
    splitDict.[(i, j)]

let rec splitCount2 (data: char array array) (i: int) (j: int): int64 =
    let counts = Array.init data.Length (fun _ -> Array.create data.[0].Length 0L)
    counts.[i].[j] <- 1

    for i in 1 .. data.Length - 1 do
        for j in 0 .. data.[i].Length - 1 do
            if data.[i].[j] = '.' then
                counts.[i].[j] <- counts.[i-1].[j]
                if j > 0 && data.[i].[j-1] = '^' then
                    counts.[i].[j] <- counts.[i].[j] + counts.[i-1].[j-1]
                if j < data.[i-1].Length - 1 && data.[i].[j+1] = '^' then
                    counts.[i].[j] <- counts.[i].[j] + counts.[i-1].[j+1]

    counts.[data.Length - 1]
    |> Array.sum

let part2 (input: string) =
    let data = parse input |> Seq.map (fun row -> Seq.toArray row) |> Seq.toArray

    let start = data.[0] |> Array.findIndex (fun c -> c = 'S')

    //splitCount data 1 start
    splitCount2 data 0 start
    //10733529153890
