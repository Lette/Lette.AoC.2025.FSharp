namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day04 =

    type Cell =
        | Empty
        | PaperRoll

    let parse input () =

        let emptyP = pchar '.' >>% Empty
        let paperRollP = pchar '@' >>% PaperRoll

        let rowP = many (emptyP <|> paperRollP)
        let rowsP = sepBy' rowP newlineP

        Parser.run rowsP input |> array2D

    let getCellAt (grid: Cell array2d) =
        let rows = Array2D.length1 grid
        let cols = Array2D.length2 grid

        fun (row, col) ->
            if row < 0 || col < 0 || row >= rows || col >= cols then
                Empty
            else
                grid[row, col]

    let neighbors =
        [
            (-1, -1); (-1, 0); (-1, 1)
            ( 0, -1);          ( 0, 1)
            ( 1, -1); ( 1, 0); ( 1, 1)
        ]

    let processCell getCellAt accFn acc row col cell =
        match cell with
        | Empty -> acc
        | PaperRoll ->
            let neighbourPaperRolls =
                neighbors
                |> List.filter (fun (dr, dc) -> getCellAt (row + dr, col + dc) = PaperRoll)
                |> List.length

            if neighbourPaperRolls < 4 then
                accFn acc row col
            else
                acc

    let part1 input =
        let getCellAt' = getCellAt input

        input |> Array2D.foldi (processCell getCellAt' (fun acc _ _ -> acc + 1)) 0

    let countRemovablePaperRolls grid =
        let getCellAt' = getCellAt grid

        let findRemovablePaperRolls () =

            grid |> Array2D.foldi (processCell getCellAt' (fun acc row col -> (row, col) :: acc)) []

        let removeRemovablePaperRolls removables =
            removables
            |> List.iter (fun (r, c) -> grid[r, c] <- Empty)

        let rec loop acc =

            let removables = findRemovablePaperRolls ()
            let numberOfRemovables = List.length removables
            if numberOfRemovables = 0 then
                acc
            else
                removeRemovablePaperRolls removables
                loop (acc + numberOfRemovables)

        loop 0

    let part2 input =

        countRemovablePaperRolls input

    let puzzle =
        Puzzle.init
            4
            (getInput >> parse)
            part1 (Some 1372)
            part2 (Some 7922)
