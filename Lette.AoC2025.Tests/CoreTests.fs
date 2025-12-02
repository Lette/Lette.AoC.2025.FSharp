namespace Lette.AoC2025

open Xunit
open FsUnit.Xunit

module CoreTests =

    [<Fact>]
    let ``Foldi works`` () =

        let array = array2D [[8; 7]; [11; 12]]

        let result = array |> Array2D.foldi (fun s x y v -> s + (x + y) * v) 0

        result |> should equal 42

    [<Fact>]
    let ``Get list of divisors`` () =
        let divisors = getDivisors 10

        divisors |> should equal [1; 2; 5; 10]

    [<Fact>]
    let ``Get list of proper divisors`` () =
        let divisors = getProperDivisors 12

        divisors |> should equal [1; 2; 3; 4; 6]
