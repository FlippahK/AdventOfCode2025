module Day2025_01

open FParsec

//let parse (input)= split "\n" input |> map (fun (s: string) -> (s.[0], int (s.[1..])))
let parse = runLineParser (pchar 'L' <|> pchar 'R' .>>. pint32)

let calcNewPos c (d, m) =
    match d with
        | 'L' -> c - m
        | 'R' -> c + m
    |> (fun c -> (c % 100 + 100) % 100)

let part1 (input: string) =
    parse input
    |> Seq.scan calcNewPos 50
    //|> Seq.iter (printfn "%i")
    |> Seq.filter (fun x -> x = 0)
    |> Seq.length

let calcNewPos2 (c, _) (d, m) =
    match d with
        | 'L' -> ((c - m) % 100 + 100) % 100, (if c > 0 && c - m <= 0 then 1 else 0) + abs(c - m) / 100
        | 'R' -> (c + m) % 100, (c + m) / 100

let part2 (input: string) =
    parse input
    |> Seq.scan calcNewPos2 (50, 0)
    |> Seq.skip 1
    |> Seq.map (fun (c, x) -> x)
    |> Seq.sum
