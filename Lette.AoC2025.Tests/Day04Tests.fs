namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day04Tests =

    open Day04

    let input = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal (array2D [
            [Empty; Empty; PaperRoll; PaperRoll; Empty; PaperRoll; PaperRoll; PaperRoll; PaperRoll; Empty]
            [PaperRoll; PaperRoll; PaperRoll; Empty; PaperRoll; Empty; PaperRoll; Empty; PaperRoll; PaperRoll]
            [PaperRoll; PaperRoll; PaperRoll; PaperRoll; PaperRoll; Empty; PaperRoll; Empty; PaperRoll; PaperRoll]
            [PaperRoll; Empty; PaperRoll; PaperRoll; PaperRoll; PaperRoll; Empty; Empty; PaperRoll; Empty]
            [PaperRoll; PaperRoll; Empty; PaperRoll; PaperRoll; PaperRoll; PaperRoll; Empty; PaperRoll; PaperRoll]
            [Empty; PaperRoll; PaperRoll; PaperRoll; PaperRoll; PaperRoll; PaperRoll; PaperRoll; Empty; PaperRoll]
            [Empty; PaperRoll; Empty; PaperRoll; Empty; PaperRoll; Empty; PaperRoll; PaperRoll; PaperRoll]
            [PaperRoll; Empty; PaperRoll; PaperRoll; PaperRoll; Empty; PaperRoll; PaperRoll; PaperRoll; PaperRoll]
            [Empty; PaperRoll; PaperRoll; PaperRoll; PaperRoll; PaperRoll; PaperRoll; PaperRoll; PaperRoll; Empty]
            [PaperRoll; Empty; PaperRoll; Empty; PaperRoll; PaperRoll; PaperRoll; Empty; PaperRoll; Empty]
        ])

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 13

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 43
