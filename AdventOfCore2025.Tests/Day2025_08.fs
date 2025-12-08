module Day2025_08

open Expecto
open Expecto.Flip

let sample = "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"

[<Tests>]
let tests =
    testList "Day 2025 08" [
        testCase "Part 1" <| fun _ ->
            Day2025_08.part1 sample
            |> Expect.equal "The expected value is " 40
        testCase "Part 2" <| fun _ ->
            Day2025_08.part2 sample
            |> Expect.equal "The expected value is " 25272
    ]