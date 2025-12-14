module Day2025_05

open Expecto
open Expecto.Flip

let sample = "3-5
10-14
16-20
12-18

1
5
8
11
17
32"


[<Tests>]
let tests =
    testList "Day 2025 05" [
        testCase "Part 1" <| fun _ ->
            Day2025_05.part1 sample
            |> Expect.equal "The expected value is " 3
        testCase "Part 2" <| fun _ ->
            Day2025_05.part2 sample
            |> Expect.equal "The expected value is " 14
    ]