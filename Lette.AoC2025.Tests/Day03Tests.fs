namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day03Tests =

    open Day03

    let input = "987654321111111
811111111111119
234234234234278
818181911112111"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal
            [
                Bank [| 9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1 |]
                Bank [| 8; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 9 |]
                Bank [| 2; 3; 4; 2; 3; 4; 2; 3; 4; 2; 3; 4; 2; 7; 8 |]
                Bank [| 8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1 |]
            ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 357L

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 3121910778619L
