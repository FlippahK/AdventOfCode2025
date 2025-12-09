module Day2025_09

open Expecto
open Expecto.Flip

let sample = "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"


[<Tests>]
let tests =
    testList "Day 2025 09" [
        testCase "Part 1" <| fun _ ->
            Day2025_09.part1 sample
            |> Expect.equal "The expected value is " 50
        testCase "Part 2" <| fun _ ->
            Day2025_09.part2 sample
            |> Expect.equal "The expected value is " 24
    ]