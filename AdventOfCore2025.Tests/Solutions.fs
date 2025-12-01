module Tests

open Expecto

[<Tests>]
let tests =
    testList "All days" [
        Day2025_01.tests;
    ]