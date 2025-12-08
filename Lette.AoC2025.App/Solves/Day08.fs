namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day08 =

    let parse input () =

        let coordP = pint32 .>> pchar ',' .>>. pint32 .>> pchar ',' .>>. pint32 |>> normalizeT
        let coordsP = rowsOf coordP

        Parser.run coordsP input

    let dist (x1, y1, z1) (x2, y2, z2) =
        let dx = int64 (x1 - x2)
        let dy = int64 (y1 - y2)
        let dz = int64 (z1 - z2)

        dx * dx + dy * dy + dz * dz

    let part1 connections junctionBoxes =

        let junctionBoxMap =
            junctionBoxes
            |> List.mapi (fun i jb -> (i, jb))
            |> Map.ofList

        let circuitMap = Array.init (List.length junctionBoxes) id

        let minList = Array.create connections (System.Int64.MaxValue, (0, 0))

        let updateMinList a b dist =
            if dist < (fst minList[connections - 1]) then
                minList[connections - 1] <- (dist, (a, b))
                Array.sortInPlaceBy fst minList

        junctionBoxMap
        |> Map.toList
        |> List.pairs
        |> List.iter (fun ((j1, p1), (j2, p2)) ->
            let dist' = dist p1 p2
            updateMinList j1 j2 dist'
        )

        let updateCircuitMap (a, b) =
            let cA = circuitMap[a]
            let cB = circuitMap[b]

            if cA <> cB then
                let minC = min cA cB
                let maxC = max cA cB
                circuitMap |> Array.iteri (fun i c ->
                    if c = maxC then
                        circuitMap[i] <- minC
                )

        for c in 0 .. (connections - 1) do
            updateCircuitMap (snd minList[c])

        circuitMap
        |> Array.toList
        |> List.countBy id
        |> List.map snd
        |> List.sortDescending
        |> List.take 3
        |> List.reduce (*)

    let part2 input =

        let input = input |> List.toArray

        let l = Array.length input

        let circuitMap = Array.init l id
        let mutable notInCircuitZero = l - 1

        let ds = seq {
            for a in 0 .. l - 1 do
                for b in (a + 1) .. l - 1 do
                    let dist' = dist input[a] input[b]
                    yield (dist', (a, b))
        }

        let dsSet = ds |> Set.ofSeq

        let updateCircuitMap (a, b) =
            let cA = circuitMap[a]
            let cB = circuitMap[b]

            if cA <> cB then
                let minC = min cA cB
                let maxC = max cA cB
                for i in 0 .. l - 1 do
                    if circuitMap[i] = maxC then begin
                        circuitMap[i] <- minC
                        if minC = 0 then
                            notInCircuitZero <- notInCircuitZero - 1
                    end

        dsSet |> Seq.pick (fun (_, (a, b)) ->
            updateCircuitMap (a, b)
            if notInCircuitZero = 0 then
                Some ((input[a] |> (fun (x, _, _) -> x)) * (input[b] |> (fun (x, _, _) -> x)))
            else
                None
        )

    let puzzle =
        Puzzle.init
            8
            (getInput >> parse)
            (part1 1000) (Some 54600)
            part2 (Some 107256172)
