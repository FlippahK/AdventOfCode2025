module Day2025_03

open FParsec

let parse = runLineParser (restOfLine false)

let inline digitOf (c: char) = int c - int '0'

let maxBatteryJoltage (s: string) =
    s
    |> Seq.mapi (fun i c -> (i, (digitOf c) * 10))
    |> Seq.collect (fun (i, first) ->
        s
        |> Seq.skip (i + 1)
        |> Seq.map (fun c -> first + digitOf c))
    |> Seq.max

let part1 (input: string) =
    parse input
    |> Seq.map maxBatteryJoltage
    |> Seq.sum
    //17535

let maxBatteryJoltage2 (i: int) (s: string): string =
    Seq.unfold (fun (k, r: string) ->
        if k = 0 then None
        elif r.Length = k then Some(r.[0], (k - 1, r.Substring 1))
        else
            let upto = r.Length - k
            let slice = r.Substring(0, upto + 1)
            let md = slice |> Seq.map digitOf |> Seq.max
            let idx = slice |> Seq.findIndex (fun c -> digitOf c = md)
            Some(r.[idx], (k - 1, r.Substring(idx + 1)))
    ) (i, s)
    |> Seq.map string
    |> String.concat ""

let part2 (input: string) =
    parse input
    |> Seq.map (maxBatteryJoltage2 12)
    |> Seq.sumBy int64
    //173577199527257