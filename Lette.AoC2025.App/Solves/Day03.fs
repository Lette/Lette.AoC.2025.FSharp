namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day03 =

    type Bank = Bank of int array

    let parse input () =

        let bankP =
            many digit
            |>> List.map (fun c -> int c - int '0')
            |>> List.toArray
            |>> Bank
        let banksP = sepBy' bankP newlineP

        Parser.run banksP input

    let bankLength (Bank digits) = digits.Length

    let findMaxDigit (Bank digits) start stop =
        let rec loop index (maxDigit, _ as acc) =
            if index > stop then
                acc
            else
                let digit = digits.[index]
                if digit > maxDigit then
                    loop (index + 1) (digit, index)
                else
                    loop (index + 1) acc

        loop start (0, 0)

    let findMaxDigits bank size =
        let length = bankLength bank

        let rec loop start stop acc =
            if stop = length then
                acc
            else
                let digit, pos = findMaxDigit bank start stop
                loop (pos + 1) (stop + 1) (digit :: acc)

        loop 0 (length - size) []
        |> List.rev

    let getJoltage size bank =
        findMaxDigits bank size
        |> List.fold (fun acc digit -> acc * 10L + int64 digit) 0L

    let sumBanks banks size =
        let rec loop banks acc =
            match banks with
            | []      -> acc
            | b :: bs -> loop bs (acc + getJoltage size b)

        loop banks 0L

    let part1 input =

        sumBanks input 2

    let part2 input =

        sumBanks input 12

    let puzzle =
        Puzzle.init
            3
            (getInput >> parse)
            part1 (Some 17405L)
            part2 (Some 171990312704598L)
