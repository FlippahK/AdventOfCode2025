module Day2025_10

open System.Collections.Generic
open Checked
open FParsec
open Microsoft.Z3

let lightsParser = pchar '[' >>. manyChars (pchar '.' <|> pchar '#') .>> pchar ']'
let innerIntsParser = sepBy pint32 (pchar ',')
let switchParser = between (pchar '(') (pchar ')') innerIntsParser
let joltageParser = between (pchar '{') (pchar '}') innerIntsParser
let parse (s: string) =
    runLineParser (lightsParser .>> spaces .>>. many (switchParser .>> spaces) .>>. joltageParser) s
    |> Seq.map (fun ((lights, switches), joltage) -> (lights, switches, joltage))

let lightsToBitsRev (lights: string) =
    lights
    |> Seq.mapi (fun i c -> if c = '#' then 1 <<< (lights.Length - i - 1) else 0)
    |> Seq.sum

let switchesToBitsRev len (switches: int list) =
    switches
    |> List.sumBy (fun switch -> 1 <<< (len - switch - 1))

let candidates = Queue<int * int>()
let rec widthFirst gen targetLights currentLights switches =
    let mutable hasMatch = false
    let nextGen = gen + 1
    for switch in switches do
        let nextLights = currentLights ^^^ switch
        if nextLights = targetLights then
            hasMatch <- true
        else
            candidates.Enqueue(nextGen, nextLights)
    if hasMatch then
        nextGen
    else
        let (nextGen, nextLights) = candidates.Dequeue()
        widthFirst nextGen targetLights nextLights switches

let part1 (input: string) =
    parse input
    |> Seq.map (fun (lights, switches, joltage) ->
    let lightBits = lightsToBitsRev lights
    let switchBitsList = switches |> List.map (switchesToBitsRev lights.Length)
    (lightBits, switchBitsList, joltage)) |> Seq.toList
    |> Seq.map (fun (lights, switches, _) ->
        candidates.Clear()
        widthFirst 0 lights 0 switches)
    |> Seq.sum
    //436

let calcPresses (switches: int list[]) (joltages: int list) =
    use ctx = new Context()
    use opt = ctx.MkOptimize()

    let presses = switches |> Array.mapi (fun i _ -> ctx.MkIntConst(sprintf "p%d" i))

    presses |> Array.iter (fun press -> opt.Add(ctx.MkGe(press, ctx.MkInt(0))))

    for i in 0 .. joltages.Length - 1 do
        let affecting =
            presses
            |> Array.mapi (fun idx p -> idx, p)
            |> Array.filter (fun (j, _) -> List.contains i switches.[j])
            |> Array.map snd

        if affecting.Length > 0 then
            let sum =
                if affecting.Length = 1 then
                    affecting.[0] :> ArithExpr
                else
                    affecting |> Array.map (fun e -> e :> ArithExpr) |> ctx.MkAdd
            opt.Add(ctx.MkEq(sum, ctx.MkInt(joltages.[i])))

    let objective =
        if presses.Length = 1 then
            presses.[0] :> ArithExpr
        else
            presses |> Array.map (fun p -> p :> ArithExpr) |> ctx.MkAdd

    opt.MkMinimize(objective) |> ignore
    opt.Check() |> ignore

    let model = opt.Model
    presses |> Array.sumBy (fun p -> (model.Eval(p, true) :?> IntNum).Int64)

let part2 (input: string) =
    parse input
    |> Seq.map (fun (_, switches, joltage) -> calcPresses (switches |> List.toArray) joltage)
    |> Seq.sum
    //14999