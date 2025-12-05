module Day2025_05

open FParsec

let parse (s: string) =
    let intervals = sepEndBy1 (pint64 .>> pchar '-' .>>. pint64) newline
    let numbers = sepEndBy1 pint64 newline
    runParser (intervals .>> newline .>>. numbers) s

let part1 (input: string) =
    parse input
    |> fun (intervals, numbers) ->
        numbers
        |> Seq.sumBy (fun p ->
            if Seq.exists (fun (f, t) -> p >= f && p <= t) intervals then 1 else 0)
    //739


let part2 (input: string) =
    let intervals = parse input |> fst |> Seq.sortBy (fun (f, _t) -> f) |> Seq.toList

    intervals
    |> List.fold (fun acc (f, t) ->
        match acc with
        | [] -> [ (f, t) ]
        | (mf, mt) :: rest -> if f <= mt then (mf, max mt t) :: rest else (f, t) :: acc
    ) []
    |> Seq.sumBy (fun (f, t) -> t - f + 1L)
    //344486348901788