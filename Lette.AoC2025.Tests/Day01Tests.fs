namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day01Tests =

    open Day01

    let input = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [Left 68; Left 30; Right 48; Left 5; Right 60; Left 55; Left 1; Left 99; Right 14; Left 82]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 3

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 6
