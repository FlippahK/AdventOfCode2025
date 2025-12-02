open Argu
open System
open System.Reflection
open DataFetch


// Define CLI argument options
type CLIArgs =
    | [<EqualsAssignment>][<First>] Day of int
    | [<EqualsAssignment>] Year of int
    | [<EqualsAssignment>] Part of int
    | [<EqualsAssignment>][<First>] RefreshDay of int
    | [<NoCommandLine>] SessionKey of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "Specify the day of the Advent of Code solution to run."
            | Year _ -> "Specify the year of the Advent of Code solution to run."
            | Part _ -> "Specify the part (1 or 2) of the day's solution to run."
            | RefreshDay _ -> "Refresh the input data for the specified day."
            | SessionKey _ -> "Specify the session key to use for fetching input data."


let getModule day year =
    try
        Some (Type.GetType($"Day{year}_{normalizeDay(day)}", throwOnError = true))
    with
    | :? TypeLoadException ->
        printfn $"Module {day} not found."
        None

let getMethod (moduleType: Type) (name: string) =
    try
        Some (moduleType.GetMethod(name))
    with
    | :? TypeLoadException ->
        printfn $"Method {name} not found."
        None

let getMethods (moduleType: Type) parts =
    let methods = moduleType.GetMethods(BindingFlags.Public ||| BindingFlags.Static)

    let whichParts =
        match parts with
        | Some 1 -> containsS "part1"
        | Some 2 -> containsS "part2"
        | None -> fun s -> containsS "part1" s || containsS "part2" s

    methods
    |> map (fun methodInfo -> methodInfo.Name)
    |> filter whichParts 
    |> map (getMethod moduleType)
    |> traverseM

[<EntryPoint>]
let main argv =
    let arguments =  ArgumentParser.Create<CLIArgs>(programName = "AdventOfCode").Parse(argv)
    printfn "Advent of Code 2025 Solutions"

    maybe {

        let dayResult = arguments.GetResult(<@ Day @>, DateTime.Now.Day)
        printfn $"Day argument: {dayResult}"
        let day = dayResult

        let yearResult = arguments.GetResult(<@ Year @>, DateTime.Now.Year)
        printfn $"Year argument: {yearResult}"
        let year = yearResult

        let part = arguments.TryGetResult <@ Part @>
        printfn $"Part argument: {part}"

        printfn $"Fetching data for day {day}..."
        let! data = getInputForDay day year
        printfn $"Data fetched successfully (length: {data.Length})"

        printfn $"Getting module for day {day}..."
        let! moduleType = getModule day year
        printfn $"Module found: {moduleType.Name}"

        printfn $"Getting methods for part {part}..."
        let! methods = getMethods moduleType part
        printfn $"Found {Seq.length methods} method(s)"

        methods
        |> iter (fun method ->
            printfn $"Running {method.Name} of {day}"
            printfn $"Answer: {method.Invoke(null, [| data |])}"
            ) 
    } |> ignore

    0