namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day01 =

    type Rotation =
        | Left of int
        | Right of int

    let parse input () =

        let leftP = pstring "L" >>. pint32 |>> Left
        let rightP = pstring "R" >>. pint32 |>> Right

        let rowP = (leftP <|> rightP)

        let rowsP = sepBy' rowP newlineP

        Parser.run rowsP input

    let part1 input =

        let rec move rotations (position, acc) =

            match rotations with
            | []            -> acc
            | Left l  :: rs -> let p = (position - l) % 100 in move rs (p, acc + (if p = 0 then 1 else 0))
            | Right r :: rs -> let p = (position + r) % 100 in move rs (p, acc + (if p = 0 then 1 else 0))

        move input (50, 0)

    let part2 input =

        let rec move rotations (position, acc) =

            match rotations with
            | []            -> acc

            | Left 0  :: rs -> move rs (position, acc)
            | Right 0 :: rs -> move rs (position, acc)

            | Left l  :: rs when l >= 100 -> move (Left  (l % 100) :: rs) (position, acc + l / 100)
            | Right r :: rs when r >= 100 -> move (Right (r % 100) :: rs) (position, acc + r / 100)

            | Left l  :: rs -> let p = (position - l) in move rs (p %+! 100, acc + (if p < 1 && position > 0 then 1 else 0))
            | Right r :: rs -> let p = (position + r) in move rs (p %+! 100, acc + (if p > 99 then 1 else 0))

        move input (50, 0)

    let puzzle =
        Puzzle.init
            1
            (getInput >> parse)
            part1 (Some 1076)
            part2 (Some 6379)
