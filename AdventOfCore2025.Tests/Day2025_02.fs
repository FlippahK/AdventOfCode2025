module Day2025_02

open Expecto
open Expecto.Flip

let sample = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

[<Tests>]
let tests =
    ptestList "Day 2025 02" [
        testCase "Part 1" <| fun _ ->
            Day2025_02.part1 sample
            |> Expect.equal "The expected value is " 1227775554L
        testCase "Part 2" <| fun _ ->
            Day2025_02.part2 sample
            |> Expect.equal "The expected value is " 4174379265L
    ]