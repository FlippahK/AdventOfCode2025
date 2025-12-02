module Day2025_02

open FParsec

let parse = runParser (sepBy (pint64 .>> pchar '-' .>>. pint64) (pchar ','))

let countValidInRange (f: int64) (t: int64) =
    seq { f .. t }
    |> Seq.filter (fun i ->
        let v = i.ToString()
        v.Substring(0, v.Length / 2) = v.Substring(v.Length / 2))
    |> Seq.sum

let part1 (input: string) =
    parse input
    |> Seq.map (fun (f, t) -> countValidInRange f t)
    |> Seq.sum
    //19386344315

let countValidInRange2 (f: int64) (t: int64) =
    seq { f .. t }
    |> Seq.filter (fun i ->
        let v = i.ToString()
        seq { 1 .. v.Length / 2 }
        |> Seq.exists (fun j ->
            v.Length % j = 0 &&
            String.replicate (v.Length / j) (v.Substring(0, j)) = v))
    |> Seq.sum

let part2 (input: string) =
    parse input
    |> Seq.map (fun (f, t) -> countValidInRange2 f t)
    |> Seq.sum
    //34421651192