namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day06 =

    let parse input () =
        // Just pass through since parsing is done differently in the two parts
        input

    let parsePart1 input =
        let spacesP = many1 spaceP
        let numbersRowP = (opt spacesP) >>. (sepBy' pint64 spacesP) .>> (opt spacesP)
        let numbersRowsP = sepBy' numbersRowP newlineP

        let operatorP = pchar '+' <|> pchar '*'
        let operatorsRowP = (opt spacesP) >>. (sepBy' operatorP spacesP) .>> (opt spacesP)

        let allP = numbersRowsP .>> newlineP .>>. operatorsRowP

        Parser.run allP input

    let part1 input =
        let numbers, operators = parsePart1 input

        let numbers' = numbers |> List.transpose
        let operators' = operators |> List.map (fun c -> if c = '+' then (+) else (*))

        let rec loop nums ops acc =
            match (nums, ops) with
            | [],      []      -> acc
            | n :: ns, o :: os -> loop ns os (acc + (List.reduce o n))
            | _                -> failwith "Mismatched lengths"

        loop numbers' operators' 0L

    let parsePart2 input =
        let rotateCcw input =
            input
            |> String.regexSplit @"\r?\n"
            |> Array.toList
            |> List.map (String.toCharArray >> Array.rev >> Array.toList)
            |> List.transpose
            |> List.map (List.toArray >> String.ofArray)
            |> String.concat "\n"

        let spacesP = many1 spaceP
        let numberP = (opt spacesP) >>. pint64 .>> (opt spacesP)
        let numbersP = sepBy' numberP newlineP
        let operatorP = pchar '+' <|> pchar '*'
        let expressionP = numbersP .>>. operatorP
        let expressionsP = sepBy' expressionP (newlineP .>> spacesP .>> newlineP)

        input
        |> rotateCcw
        |> Parser.run expressionsP

    let part2 input =
        let input' = parsePart2 input

        input'
        |> List.map (fun (nums, op) ->
            let operation = if op = '+' then (+) else (*)
            List.reduce operation nums
        )
        |> List.sum

    let puzzle =
        Puzzle.init
            6
            (getInput >> parse)
            part1 (Some 6299564383938L)
            part2 (Some 11950004808442L)
