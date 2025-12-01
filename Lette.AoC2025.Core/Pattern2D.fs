namespace Lette.AoC2025

open Microsoft.FSharp.Core.Operators.Checked

type Pattern2D = (int * int * char) list

[<RequireQualifiedAccess>]
module Pattern2D =

    let private height (pattern: Pattern2D) =
        pattern |> Seq.map (fun (x, _, _) -> x) |> Seq.max |> (+) 1

    let private width (pattern: Pattern2D) =
        pattern |> Seq.map (fun (_, y, _) -> y) |> Seq.max |> (+) 1

    let scan pattern map =

        let mapHeight = Array2D.length1 map
        let mapWidth = Array2D.length2 map

        let patternHeight = height pattern
        let patternWidth = width pattern

        let rec loopMap row col acc =
            match row, col with
            | r, _ when r > (mapHeight - patternHeight) -> acc
            | _, c when c > (mapWidth - patternWidth)   -> loopMap (row + 1) 0 acc
            | r, c ->
                let rec loopPattern ps =
                    match ps with
                    | [] -> true
                    | (x, y, ch) :: ps' ->
                        if Array2D.get map (r + x) (c + y) <> ch then
                            false
                        else
                            loopPattern ps'

                if loopPattern pattern then
                    loopMap row (col + 1) ((r, c) :: acc)
                else
                    loopMap row (col + 1) acc

        loopMap 0 0 []

    let scanMany patterns map =
        let rec loop patterns acc =
            match patterns with
            | [] -> acc
            | p :: ps ->
                loop ps (scan p map @ acc)

        loop patterns []
