module Day2025_06

open Expecto
open Expecto.Flip

let sample = "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  "


[<Tests>]
let tests =
    testList "Day 2025 06" [
        testCase "Part 1" <| fun _ ->
            Day2025_06.part1 sample
            |> Expect.equal "The expected value is " 4277556
        testCase "Part 2" <| fun _ ->
            Day2025_06.part2 sample
            |> Expect.equal "The expected value is " 3263827
    ]