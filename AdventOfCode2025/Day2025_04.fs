module Day2025_04

open FParsec

let parse = runLineParser (many (pchar '.' <|> pchar '@'))

let countMovable (grid: char[][]) =
    let neighbors = [|(-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1)|]
    let mutable count = 0
    for i in 0 .. grid.Length - 1 do
        for j in 0 .. grid.[i].Length - 1 do
            if grid.[i].[j] = '@' then
                let neighborCount =
                    neighbors
                    |> Array.sumBy (fun (di, dj) ->
                        let ni, nj = i + di, j + dj
                        if ni >= 0 && ni < grid.Length && nj >= 0 && nj < grid.[ni].Length && grid.[ni].[nj] <> '.' then 1 else 0)
                if neighborCount < 4 then
                    grid.[i].[j] <- 'x';
                    count <- count + 1
    count

let part1 (input: string) =
    parse input
    |> Seq.map (Seq.toArray)
    |> Seq.toArray
    |> countMovable
    //1433

let commitGrid (grid: char[][]) =
    for i in 0 .. grid.Length - 1 do
        for j in 0 .. grid.[i].Length - 1 do
            if grid.[i].[j] = 'x' then
                grid.[i].[j] <- '.'

let part2 (input: string) =
    let data = parse input |> Seq.map (Seq.toArray) |> Seq.toArray
    Seq.unfold (fun grid ->
        let c = countMovable grid
        if c = 0 then None
        else
            commitGrid grid
            Some(c, grid)
    ) data
    |> Seq.sum
    //8616
