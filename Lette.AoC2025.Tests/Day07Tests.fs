namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day07Tests =

    open Day07

    let input = ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal (array2D [
            [ Air; Air; Air; Air; Air; Air; Air; Start; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Splitter; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Splitter; Air; Splitter; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Splitter; Air; Splitter; Air; Splitter; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Splitter; Air; Splitter; Air; Air; Air; Splitter; Air; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Air; Splitter; Air; Splitter; Air; Air; Air; Splitter; Air; Splitter; Air; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Air; Splitter; Air; Air; Air; Splitter; Air; Air; Air; Air; Air; Splitter; Air; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
            [ Air; Splitter; Air; Splitter; Air; Splitter; Air; Splitter; Air; Splitter; Air; Air; Air; Splitter; Air ]
            [ Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air; Air ]
        ])

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 21

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 40L
