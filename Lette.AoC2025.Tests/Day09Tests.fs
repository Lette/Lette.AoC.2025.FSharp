namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day09Tests =

    open Day09

    let input = "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal
            [
                (7L, 1L)
                (11L, 1L)
                (11L, 7L)
                (9L, 7L)
                (9L, 5L)
                (2L, 5L)
                (2L, 3L)
                (7L, 3L)
            ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 50L

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 24L
