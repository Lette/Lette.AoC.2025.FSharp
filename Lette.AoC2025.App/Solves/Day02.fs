namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day02 =

    type Range = Range of First: int64 * Last: int64

    let parse input () =

        let rangeP = pint64 .>> pchar '-' .>>. pint64 |>> Range
        let rangesP = sepBy1 rangeP (pchar ',')

        Parser.run rangesP input

    let sumInvalidIds isInvalid ranges =

        let rec loopRange (Range (first, last)) acc =
            let rec loopId id acc =
                if id > last then
                    acc
                else if (isInvalid id) then
                    loopId (id + 1L) (acc + id)
                else
                    loopId (id + 1L) acc

            loopId first acc

        let rec loopRanges ranges acc =
            match ranges with
            | []          -> acc
            | range :: rs -> loopRanges rs (loopRange range acc)

        loopRanges ranges 0L

    let splitIntoParts (s: string) l partLength =
        [ for i in 0 .. (l / partLength) - 1 -> let index = i * partLength in s[index .. index + partLength - 1] ]

    let areAllPartsEqual parts =
        match parts with
        | []      -> failwith "Shouldn't happen"
        | _ :: [] -> failwith "Shouldn't happen"
        | x :: xs -> xs |> List.forall (fun p -> p = x)

    let part1 input =

        let isInvalid id =
            let s = string id
            let d = s |> String.length

            if isOdd d then
                false
            else
                s[..(d / 2 - 1)] = s[(d / 2)..]

        sumInvalidIds isInvalid input

    let part2 input =

        let isInvalid id =
            let s = string id
            let l = s |> String.length
            let divisors = getProperDivisors l

            let findInvalidWithDivisor divisor =
                splitIntoParts s l divisor
                |> areAllPartsEqual

            let rec findInvalid divisors acc =
                match (divisors, acc) with
                | _      , true -> true
                | []     , _    -> false
                | x :: xs, _    -> findInvalid xs (findInvalidWithDivisor x)

            findInvalid divisors false

        sumInvalidIds isInvalid input

    let puzzle =
        Puzzle.init
            2
            (getInput >> parse)
            part1 (Some 23701357374L)
            part2 (Some 34284458938L)
