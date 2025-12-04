module Day2025_04

open Expecto
open Expecto.Flip

let sample = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."

[<Tests>]
let tests =
    ptestList "Day 2025 04" [
        testCase "Part 1" <| fun _ ->
            Day2025_04.part1 sample
            |> Expect.equal "The expected value is " 13
        testCase "Part 2" <| fun _ ->
            Day2025_04.part2 sample
            |> Expect.equal "The expected value is " 43
    ]