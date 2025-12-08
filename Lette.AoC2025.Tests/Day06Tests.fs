namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day06Tests =

    open Day06

    let input =
        "123 328  51 64 \n" +
        " 45 64  387 23 \n" +
        "  6 98  215 314\n" +
        "*   +   *   +  "

    [<Fact>]
    let ``Parser part 1 works`` () =
        let result = parsePart1 input

        result |> should equal (
            [
                [123L; 328L; 51L; 64L]
                [45L; 64L; 387L; 23L]
                [6L; 98L; 215L; 314L]
            ],
            ['*'; '+'; '*'; '+']
        )

    [<Fact>]
    let ``Parser part 2 works`` () =
        let result = parsePart2 input

        result |> should equal (
            [
                ([4L; 431L; 623L], '+')
                ([175L; 581L; 32L], '*')
                ([8L; 248L; 369L], '+')
                ([356L; 24L; 1L], '*')
            ]
        )
    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 4277556L

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 3263827L
