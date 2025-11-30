module DataFetch

open System
open FsHttp.DslCE
open FsHttp.Response
open System.IO
open FsHttp.Request
open FSharp.Configuration

//let private getSessionKey() =
//    match Environment.GetEnvironmentVariable("AOC_SESSION") with
//    | null | "" ->
//        let configPath = Path.Combine(__SOURCE_DIRECTORY__, "app.config")
//        if File.Exists(configPath) then
//            let xml = System.Xml.Linq.XDocument.Load(configPath)
//            let sessionKey = 
//                xml.Descendants(System.Xml.Linq.XName.Get("add"))
//                |> Seq.tryFind (fun el -> el.Attribute(System.Xml.Linq.XName.Get("key")).Value = "SessionKey")
//                |> Option.map (fun el -> el.Attribute(System.Xml.Linq.XName.Get("value")).Value)
//            match sessionKey with
//            | Some key when not (String.IsNullOrWhiteSpace(key)) -> key
//            | _ -> failwith "SessionKey not found in app.config. Add a 'SessionKey' entry or set the AOC_SESSION environment variable."
//        else
//            failwith "app.config not found and AOC_SESSION environment variable not set. Please add one of these."
//    | key -> key

type Settings = AppSettings<"app.config">

let fetchData day year = 

    //let sessionKey = getSessionKey()
    let sessionKey = Settings.SessionKey
    
    printfn $"Fetching input for Day{day} using session key {sessionKey}."

    http {
        GET $"https://adventofcode.com/{year}/day/{day}/input"
        Cookie "session" sessionKey
    }
    |> send
    |> toTextAsync 
    |> Async.RunSynchronously

let getInputForDay (day: int) (year: int) =
    let filePath = Path.Combine("data", $"Day{year}_{normalizeDay(day)}.txt")

    if File.Exists(filePath) then
        printfn $"Input file for Day{day} already exists. Reading from local file."
        Some <| File.ReadAllText(filePath)
    else
        printfn $"Input file for Day{day} does not exist locally. Fetching from AoC website."
        
        try
            let input = fetchData day year

            File.WriteAllText(filePath, input)
            Some input
        with
        | e ->
            printfn $"Error fetching input for Day{day}: {e.Message}"
            None

let refreshInputForDay day =
    let filePath = Path.Combine("data", $"Day{day}.txt")

    if File.Exists(filePath) then
        File.Delete(filePath)
        printfn $"Input file for Day{day} deleted."
    else
        printfn $"Input file for Day{day} does not exist locally."

    getInputForDay day