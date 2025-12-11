module Day2025_11

open Expecto
open Expecto.Flip

let sample = "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"

let sample2 = "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out"

[<Tests>]
let tests =
    testList "Day 2025 11" [
        testCase "Part 1" <| fun _ ->
            Day2025_11.part1 sample
            |> Expect.equal "The expected value is " 5
        testCase "Part 2" <| fun _ ->
            Day2025_11.part2 sample2
            |> Expect.equal "The expected value is " 2
    ]