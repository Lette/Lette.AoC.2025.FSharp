namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

type Range = { Start: int64; Stop: int64 }

module Range =

    let create start stop = { Start = start; Stop = stop }

    let contains value range =
        range.Start <= value && value <= range.Stop

    let size range =
        range.Stop - range.Start + 1L

module Day05 =

    let parse input () =

        let rangeP = pint64 .>> pchar '-' .>>. pint64 ||>> Range.create
        let rangesP = sepBy' rangeP newlineP

        let idP = pint64
        let idsP = sepBy' idP newlineP

        let allP = rangesP .>> newlineP .>> newlineP .>>. idsP

        Parser.run allP input

    let part1 (ranges, ids) =

        let rec countFreshIngredients ids count =
            match ids with
            | [] -> count
            | id :: ids' ->
                let isFresh = ranges |> List.exists (Range.contains id)
                countFreshIngredients ids' (if isFresh then count + 1 else count)

        countFreshIngredients ids 0

    let part2 (ranges, _) =

        let idCounter (count, pos) range =
            if range.Stop <= pos then
                (count, pos)
            else if range.Start > pos then
                (count + Range.size range, range.Stop)
            else
                let start' = max (pos + 1L) range.Start
                let range' = { range with Start = start' }
                (count + Range.size range', range'.Stop)

        ranges
        |> List.sortBy _.Start
        |> List.fold idCounter (0L, 0L)
        |> fst

    let puzzle =
        Puzzle.init
            5
            (getInput >> parse)
            part1 (Some 770)
            part2 (Some 357674099117260L)
