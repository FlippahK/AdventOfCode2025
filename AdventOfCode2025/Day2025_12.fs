module Day2025_12

open System
open Checked
open FParsec

let statsLine = pint32 .>> pchar '-' .>>. pint32 .>> pchar ':' .>> spaces .>>. (many (pint32 .>> spaces))

let part1 (input: string) =
    let data = input.Split([|"\n\n"; "\r\n\r\n"|], StringSplitOptions.None)

    let tiles = data.[..data.Length - 2] |> Array.map (fun block ->
        let lines = block.Split([|"\n"; "\r\n"|], StringSplitOptions.None)
        let id = lines.[0].TrimEnd(':') |> int
        let tileLines = lines.[1..]
        (id, tileLines)
    )
    printfn "%A" tiles

    let stats = data.[data.Length - 1].Replace("x", "-").Split([|"\n"; "\r\n"|], StringSplitOptions.RemoveEmptyEntries) |> Seq.map (runParser statsLine)
    printfn "%A" stats

    let mutable good = 0
    let mutable bad = 0
    for ((x, y), n) in stats do
        let max = n |> Seq.mapi (fun i v -> v * (tiles.[i] |> snd |> Seq.sumBy (fun s -> s.Length))) |> Seq.sum
        let min = n |> Seq.mapi (fun i v -> v * (tiles.[i] |> snd |> Seq.sumBy (fun s -> s |> Seq.filter (fun c -> c = '#') |> length))) |> Seq.sum
        if x * y >= max then good <- good + 1
        if x * y <= min then bad <- bad + 1

    printfn "Good: %d, Bad: %d" good bad
        

    // 510 bad
    // 490 good
    // Turns out that all are trivially either good or bad so the answer is just the count of good ones
    good

let part2 (input: string) =
    printfn "Today had no part 2"
    0
