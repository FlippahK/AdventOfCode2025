module Day2025_03

open Expecto
open Expecto.Flip

let sample = "987654321111111
811111111111119
234234234234278
818181911112111"

[<Tests>]
let tests =
    ptestList "Day 2025 03" [
        testCase "Part 1" <| fun _ ->
            Day2025_03.part1 sample
            |> Expect.equal "The expected value is " 357
        testCase "Part 2" <| fun _ ->
            Day2025_03.part2 sample
            |> Expect.equal "The expected value is " 0
    ]