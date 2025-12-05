namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day05 =

    let parse input () =

        let rangeP = pint64 .>> pchar '-' .>>. pint64
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
                let isFresh =
                    ranges
                    |> List.exists (fun (start, stop) -> start <= id && id <= stop)

                countFreshIngredients ids' (if isFresh then count + 1 else count)

        countFreshIngredients ids 0

    let part2 (ranges, _) =

        let idCounter (count, pos) (start, stop) =
            if stop <= pos then
                (count, pos)
            else if start > pos then
                (count + stop - start + 1L, stop)
            else
                let start' = max (pos + 1L) start
                (count + stop - start' + 1L, stop)

        ranges
        |> List.sortBy fst
        |> List.fold idCounter (0L, 0L)
        |> fst

    let puzzle =
        Puzzle.init
            5
            (getInput >> parse)
            part1 (Some 770)
            part2 (Some 357674099117260L)
