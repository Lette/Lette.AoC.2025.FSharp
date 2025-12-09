namespace Lette.AoC2025

open System
open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day09 =

    let parse input () =

        let coordP = pint64 .>> pchar ',' .>>. pint64
        let coordsP = rowsOf coordP

        Parser.run coordsP input

    let part1 input =

        let findMaxArea maxArea ((x1, y1), (x2, y2)) =

            let dx = abs (x1 - x2) + 1L
            let dy = abs (y1 - y2) + 1L

            let area = dx * dy

            if area > maxArea then
                area
            else
                maxArea

        input
        |> List.pairs
        |> List.fold findMaxArea 0L

    let move (x, y) (dx, dy) = (x + dx, y + dy)
    let turnRight (dx, dy) = (-dy, dx)
    let turnLeft (dx, dy) = (dy, -dx)

    let isRightTurn direction (x1, y1) (x2, y2) =
        match direction with
        |  0, -1 -> x2 > x1
        |  1,  0 -> y2 > y1
        |  0,  1 -> x2 < x1
        | -1,  0 -> y2 < y1
        | _ -> failwith "invalid direction"

    let offsetAtRightTurn direction =
        match direction with
        |  0, -1 -> (-1L, -1L)
        |  1,  0 -> ( 1L, -1L)
        |  0,  1 -> ( 1L,  1L)
        | -1,  0 -> (-1L,  1L)
        | _ -> failwith "invalid direction"

    let offsetAtLeftTurn direction =
        match direction with
        |  0, -1 -> (-1L,  1L)
        |  1,  0 -> (-1L, -1L)
        |  0,  1 -> ( 1L, -1L)
        | -1,  0 -> ( 1L,  1L)
        | _ -> failwith "invalid direction"

    let edgesFromSquare ((x1, y1), (x2, y2)) =
        (
            // horizontal edges
            ((x1, y1), (x2, y1)),
            ((x1, y2), (x2, y2)),
            // vertical edges
            ((x1, y1), (x1, y2)),
            ((x2, y1), (x2, y2))
        )

    let part2 input =

        let minY = input |> List.fold (fun minY (_, y) -> min minY y) Int64.MaxValue

        let segmentWalkStart =
            input
            |> List.where (fun (_, y) -> y = minY) |> List.minBy snd

        let input' =
            input @ input
            |> List.skipWhile (fun p1 -> p1 <> segmentWalkStart)
            |> List.take (List.length input + 1)

        let rec walkOutside points direction acc =
            match points with
            | _ :: [] -> acc
            | a :: b :: ps ->
                if isRightTurn direction a b then
                    let offset = offsetAtRightTurn direction
                    let o' = move a offset
                    walkOutside (b :: ps) (turnRight direction) (o' :: acc)
                else
                    let offset = offsetAtLeftTurn direction
                    let o' = move a offset
                    walkOutside (b :: ps) (turnLeft direction) (o' :: acc)
            | _ -> failwith "should never happen"

        let outsidePoints =
            walkOutside input' (0, -1) []

        let outsideSegments =
            outsidePoints
            |> List.windowed 2
            |> List.map (function
                | [(x1, y1); (x2, y2)] -> ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))
                | _ -> failwith "should never happen"
            )

        let outsideHorizontals, outsideVerticals =
            outsideSegments
            |> List.partition (fun ((_, y1), (_, y2)) -> y1 = y2)

        let squares =
            input
            |> List.pairs
            |> List.map (fun ((x1, y1), (x2, y2)) -> ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2)))

        let linesIntersect horizontal vertical =
            let (x1, y), (x2, _) = horizontal
            let (x, y1), (_, y2) = vertical
            x1 <= x && x <= x2 && y1 <= y && y <= y2

        let validSquares =
            squares
            |> List.filter (fun square ->
                let h1, h2, v1, v2 = edgesFromSquare square

                outsideVerticals
                |> List.forall (fun ov -> not (linesIntersect h1 ov) && not (linesIntersect h2 ov))

                &&

                outsideHorizontals
                |> List.forall (fun oh -> not (linesIntersect oh v1) && not (linesIntersect oh v2))
            )

        validSquares
        |> List.map (fun ((x1, y1), (x2, y2)) -> ((x2 - x1 + 1L) * (y2 - y1 + 1L)))
        |> List.max

    let puzzle =
        Puzzle.init
            9
            (getInput >> parse)
            part1 (Some 4763509452L)
            part2 (Some 1516897893L)
