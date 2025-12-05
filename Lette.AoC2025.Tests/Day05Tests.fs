namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day05Tests =

    open Day05

    let input = "3-5
10-14
16-20
12-18

1
5
8
11
17
32"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal ([(3L, 5L); (10L, 14L); (16L, 20L); (12L, 18L)], [1L; 5L; 8L; 11L; 17L; 32L])

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 3

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 14L
