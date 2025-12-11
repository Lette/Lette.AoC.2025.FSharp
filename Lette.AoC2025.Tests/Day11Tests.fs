namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module Day11Tests =

    open Day11

    let input = "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"

    [<Fact>]
    let ``Parser works`` () =
        let result = parse input ()

        result |> should equal [
            ("aaa", ["you"; "hhh"])
            ("you", ["bbb"; "ccc"])
            ("bbb", ["ddd"; "eee"])
            ("ccc", ["ddd"; "eee"; "fff"])
            ("ddd", ["ggg"])
            ("eee", ["out"])
            ("fff", ["out"])
            ("ggg", ["out"])
            ("hhh", ["ccc"; "fff"; "iii"])
            ("iii", ["out"])
        ]

    [<Fact>]
    let ``Part 1`` () =

        let result = part1 (parse input ())

        result |> should equal 5L

    [<Fact>]
    let ``Part 2`` () =

        let input = "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out"

        let result = part2 (parse input ())

        result |> should equal 2L
