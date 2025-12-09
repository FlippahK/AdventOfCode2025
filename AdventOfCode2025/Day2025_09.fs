module Day2025_09

open Checked
open FParsec
open NetTopologySuite
open NetTopologySuite.Geometries

let parse (s: string) = runLineParser (pint64 .>> pchar ',' .>>. pint64) s

let part1 (input: string) =
    let data = parse input |> Seq.toArray

    data
    |> Array.mapi (fun i (x, y) ->
        data.[i+1..]
        |> Array.map (fun (xx, yy) ->
            //((x, y, xx, yy), abs(x - xx) * abs(y - yy))
            (abs(x - xx) + 1L) * (abs(y - yy) + 1L)
        )
    )
    |> Array.concat
    |> Array.sortByDescending id
    |> Array.head
    //4769758290

let part2 (input: string) =
    let data = parse input |> Seq.toArray

    let factory = NtsGeometryServices.Instance.CreateGeometryFactory()
    let polygonCoords = Array.append data [| data.[0] |]
    let polygon = factory.CreatePolygon(polygonCoords |> Array.map (fun (x, y) -> Coordinate(float x, float y)))

    data
    |> Array.mapi (fun i (x, y) ->
        data.[i+1..]
        |> Array.map (fun (xx, yy) ->
            //((x, y, xx, yy), abs(x - xx) * abs(y - yy))
            ((abs(x - xx) + 1L) * (abs(y - yy) + 1L), [| Coordinate(float x, float y); Coordinate(float xx, float y); Coordinate(float xx, float yy); Coordinate(float x, float yy); Coordinate(float x, float y) |])
        )
    )
    |> Array.concat
    |> Array.sortByDescending fst
    |> Array.skipWhile (fun (_, coords) -> not (polygon.Contains(factory.CreatePolygon(coords))))
    |> Array.head
    |> fst
    //1588990708
