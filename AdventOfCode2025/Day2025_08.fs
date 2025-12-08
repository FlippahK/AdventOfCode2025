module Day2025_08

open Checked
open FParsec

let parse (s: string) = runLineParser (sepBy pint64 (pchar ',')) s

let connect (res: ResizeArray<ResizeArray<(int64 * int64 * int64)>>) nodeCount ((x1, y1, z1, x2, y2, z2), _: int64) =
    let m1 = res |> Seq.tryFind (fun group -> Seq.exists (fun (x, y, z) -> (x = x1 && y = y1 && z = z1)) group)
    let m2 = res |> Seq.tryFind (fun group -> Seq.exists (fun (x, y, z) -> (x = x2 && y = y2 && z = z2)) group)

    if m1.IsSome && m1 = m2 then
        ()
    elif m1.IsSome && m2.IsSome then
        let g1 = m1.Value
        let g2 = m2.Value
        res.Remove(g2) |> ignore
        g1.AddRange(g2)
    elif m1.IsSome then
        let g1 = m1.Value
        g1.Add((x2, y2, z2))
    elif m2.IsSome then
        let g2 = m2.Value
        g2.Add((x1, y1, z1))
    else
        let newGroup = ResizeArray<(int64 * int64 * int64)>()
        newGroup.Add((x1, y1, z1))
        newGroup.Add((x2, y2, z2))
        res.Add(newGroup)

    if res.Count = 1 && res.[0].Count = nodeCount then
        Some (x1, x2)
    else
        None

let part1 (input: string) =
    let res = ResizeArray<ResizeArray<(int64 * int64 * int64)>>()
    let data = parse input |> Seq.toArray |> Array.map (function [x; y; z] -> [|x; y; z|])
    let connectionSize = if data.Length < 1000 then 10 else 1000

    data
    |> Array.mapi (fun i [|x; y; z|] ->
        data.[i+1..]
        |> Array.map (fun [|xx; yy; zz|] ->
            ((x, y, z, xx, yy, zz), ((x - xx) * (x - xx) + (y - yy) * (y - yy) + (z - zz) * (z - zz)))
        )
    )
    |> Array.concat
    |> Array.sortBy snd
    |> Array.take connectionSize
    |> Array.iter (fun c -> connect res -1 c |> ignore)

    res
    |> Seq.map (fun a -> a.Count)
    |> Seq.sortByDescending id
    |> Seq.take 3
    |> Seq.fold (fun acc c -> acc * int64 c) 1L
    //80446

let part2 (input: string) =
    let res = ResizeArray<ResizeArray<(int64 * int64 * int64)>>()
    let data = parse input |> Seq.toArray |> Array.map (function [x; y; z] -> [|x; y; z|])

    data
    |> Array.mapi (fun i [|x; y; z|] ->
        data.[i+1..]
        |> Array.map (fun [|xx; yy; zz|] ->
            ((x, y, z, xx, yy, zz), ((x - xx) * (x - xx) + (y - yy) * (y - yy) + (z - zz) * (z - zz)))
        )
    )
    |> Array.concat
    |> Array.sortBy snd
    |> Array.tryPick (fun c -> connect res data.Length c)
    |> Option.defaultWith (fun () -> failwith "No connection produced a result")
    |> fun (x1, x2) -> x1 * x2
    //51294528
