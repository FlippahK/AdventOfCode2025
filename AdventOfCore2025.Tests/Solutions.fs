module Tests

open Expecto

[<Tests>]
let tests =
    testList "All days" [
        Day2025_01.tests;
        Day2025_02.tests;
        Day2025_03.tests;
        Day2025_04.tests;
    ]