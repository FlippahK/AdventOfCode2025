module Day2025_01

open Expecto
open Expecto.Flip

let sample = 
    """L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"""

[<Tests>]
let tests =
    testList "Day 2025 01" [
        testCase "Part 1" <| fun _ ->
            Day2025_01.part1 sample
            |> Expect.equal "The expected value is " 3
        testCase "Part 2" <| fun _ ->
            Day2025_01.part2 sample
            |> Expect.equal "The expected value is "  6
    ]