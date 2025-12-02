namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day02Tests =

    open Day02

    let input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [Range (11, 22); Range (95, 115); Range (998, 1012); Range (1188511880, 1188511890); Range (222220, 222224); Range (1698522, 1698528); Range (446443, 446449); Range (38593856, 38593862); Range (565653, 565659); Range (824824821, 824824827); Range (2121212118, 2121212124)]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 1227775554L

    [<Fact>]
    let ``Part 2`` () =

        let result = part2 (parse input ())

        result |> should equal 4174379265L
