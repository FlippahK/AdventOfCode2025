module Day2025_10

open Expecto
open Expecto.Flip

let sample = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"


[<Tests>]
let tests =
    testList "Day 2025 10" [
        testCase "Part 1" <| fun _ ->
            Day2025_10.part1 sample
            |> Expect.equal "The expected value is " 7
        testCase "Part 2" <| fun _ ->
            Day2025_10.part2 sample
            |> Expect.equal "The expected value is " 33
    ]