namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day07 =

    type Cell =
        | Start
        | Air
        | Splitter

    let parse input () =
        let startP = pchar 'S' >>% Start
        let airP = pchar '.' >>% Air
        let splitterP = pchar '^' >>% Splitter
        let cellP = startP <|> airP <|> splitterP

        let rowP = many cellP
        let rowsP = sepBy' rowP newlineP

        Parser.run rowsP input |> array2D

    let part1 input =
        let height = Array2D.length1 input
        let width = Array2D.length2 input

        let rec loopCols row (prevBeams, splits) =

            let rec loopCol col (currBeams, splits) =
                if col = width then
                    (currBeams, splits)
                else
                    let cell = Array2D.get input row col
                    let above = Array2D.get input (row - 1) col

                    match cell, above, (Set.contains col prevBeams) with
                    | _,        Start, _    -> loopCol (col + 1) (Set.add col currBeams,                               splits)
                    | Air,      Air,   true -> loopCol (col + 1) (Set.add col currBeams,                               splits)
                    | Splitter, Air,   true -> loopCol (col + 1) (currBeams |> Set.add (col - 1) |> Set.add (col + 1), splits + 1)
                    | _                     -> loopCol (col + 1) (currBeams,                                           splits)

            loopCol 0 (Set.empty, splits)

        let rec loopRows row (beams, splits) =
            if row = height then
                splits
            else
                loopRows (row + 1) (loopCols row (beams, splits))

        loopRows 1 (Set.empty, 0)

    let part2 input =

        let height = Array2D.length1 input
        let width = Array2D.length2 input

        let getCellAt (x, y) = Array2D.get input x y

        let routes = Array2D.init height width (fun r _ -> if r = height - 1 then 1L else 0L)
        let setRouteAt (x, y) value = Array2D.set routes x y value
        let getRouteAt (x, y) = Array2D.get routes x y

        let startPos = Array2D.find Start input |> (fun (row, col) -> (row + 1, col))

        let rec loopRoutes row col =
            if row = -1 then
                ()
            else if col = width then
                loopRoutes (row - 1) 0
            else
                let cell = getCellAt (row, col)
                match cell with
                | Splitter -> setRouteAt (row, col) (getRouteAt (row + 1, col - 1) + getRouteAt (row + 1, col + 1))
                | _        -> setRouteAt (row, col) (getRouteAt (row + 1, col))
                loopRoutes row (col + 1)

        loopRoutes (height - 2) 0

        getRouteAt startPos

    let puzzle =
        Puzzle.init
            7
            (getInput >> parse)
            part1 (Some 1698)
            part2 (Some 95408386769474L)
