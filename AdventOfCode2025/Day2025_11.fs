module Day2025_11

open System.Collections.Generic
open FParsec

let parse (s: string) = runLineParser (anyString 3 .>> pchar ':' .>> spaces .>>. many (anyString 3 .>> spaces)) s

let rec findPath (data: IDictionary<string, string list>) (node: string) (visited: Set<string>) =
    match node with
    | "out" -> 1
    | _ ->
        data[node]
        |> Seq.filter (fun m -> not (Set.contains m visited))
        |> Seq.map (fun m -> findPath data m (visited.Add m))
        |> Seq.sum

let part1 (input: string) =
    let data = parse input |> dict

    let startNode = "you"
    let visited = set [startNode]
    findPath data startNode visited
    //701

let rec findPath2 (data: IDictionary<string, string list>) (memoize: Dictionary<(string * bool * bool), int64>) (node: string) (visited: Set<string>) visitedDac visitedFft =
    if memoize.ContainsKey (node, visitedDac, visitedFft) then
        memoize.[(node, visitedDac, visitedFft)]
    else
        match node with
        | "out" ->
            let res = if (visitedDac && visitedFft) then 1L else 0L
            memoize.Add((node, visitedDac, visitedFft), res)
            res
        | _ ->
            let found =
                data[node]
                |> Seq.filter (fun m -> not (Set.contains m visited))
                |> Seq.map (fun m -> findPath2 data memoize m (visited.Add m) (visitedDac || node = "dac") (visitedFft || node = "fft"))
                |> Seq.sum
            memoize.Add((node, visitedDac, visitedFft), found)
            found

let part2 (input: string) =
    let data = parse input |> dict

    let startNode = "svr"
    let visited = set [startNode]
    let memoize = Dictionary<(string * bool * bool), int64>()
    findPath2 data memoize startNode visited false false
    //390108778818526