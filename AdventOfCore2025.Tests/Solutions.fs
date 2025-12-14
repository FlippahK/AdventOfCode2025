module Tests

open Expecto

[<Tests>]
let tests =
    testList "All days" [
        Day2025_01.tests;
        Day2025_02.tests;
        Day2025_03.tests;
        Day2025_04.tests;
        Day2025_05.tests;
        Day2025_06.tests;
        Day2025_07.tests;
        Day2025_08.tests;
        Day2025_09.tests;
        Day2025_10.tests;
        Day2025_11.tests;
    ]