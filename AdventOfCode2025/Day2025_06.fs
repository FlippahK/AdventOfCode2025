module Day2025_06

open FParsec

let parse (s: string) =
    let parts = s.Split('\n', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> x.TrimEnd())
    let numbersParser = many1 (pint64 .>> spaces)
    let opsParser = many1 (pchar '+' <|> pchar '*' .>> spaces)
    let numbers = parts.[0..parts.Length-2] |> Seq.map (fun line -> runParser numbersParser (line.Trim()))
    let ops = parts.[parts.Length-1] |> runParser opsParser
    (numbers, ops)

let applyOp (op: char) (nums: seq<int64>) : int64 =
    match op with
    | '+' -> nums |> Seq.sum
    | '*' -> nums |> Seq.fold (fun acc v -> acc * v) 1L

let part1 (input: string) =
    let numbers, ops = parse input |> fun (n, o) -> (n |> Seq.map (fun a -> a |> Seq.toArray) |>Seq.toArray, o)
    ops
    |> Seq.mapi (fun i op ->
        numbers
        |> Array.map (fun row -> row.[i])
        |> applyOp op)
    |> Seq.sum
    //6171290547579

let parse2 (s: string) =
    let parts = s.Split('\n', System.StringSplitOptions.RemoveEmptyEntries)
    let opsParser = many1 (pchar '+' <|> pchar '*' .>>. (manyChars (pchar ' ')))
    let ops = parts.[parts.Length - 1] |> runParser opsParser |> Array.ofSeq
    let numbers = ResizeArray<ResizeArray<string>>()
    for line in parts.[0..parts.Length - 2] do
        let row = ResizeArray<string>()
        let mutable idx = 0
        for col in 0 .. ops.Length - 1 do
            let size = (ops.[col] |> snd |> length) + if col = ops.Length - 1 then 1 else 0
            let chunk = line.Substring(idx, size)
            row.Add(chunk)
            idx <- idx + 1 + size
        numbers.Add(row)
    (numbers, ops |> Array.map (fun (o, s) -> o))

let part2 (input: string) =
    let numbers, ops = parse2 input
    let mutable res = 0L
    for i in 0 .. ops.Length - 1 do
        let nums = ResizeArray<int64>()
        for j in 0 .. numbers.[0].[i].Length - 1 do
            nums.Add(numbers |> Seq.map (fun n -> n.[i].[j]) |> Seq.toArray |> System.String |> int64)
        res <- res + (applyOp ops.[i] nums)
    res
    //8811937976367