namespace Lette.AoC2025

open FParsec
open Microsoft.FSharp.Core.Operators.Checked

module Day11 =

    let parse input () =

        let nodeP = anyString 3
        let rowP = nodeP .>> pstring ": " .>>. sepBy' nodeP spaceP
        let rowsP = rowsOf rowP

        Parser.run rowsP input

    let kahnTopologicalSort (adjacencyList: Map<string, string list>) =
        let allNodes =
            adjacencyList
            |> Map.toList
            |> List.collect (fun (node, neighbors) -> node :: neighbors)
            |> List.distinct

        let inDegree =
            allNodes
            |> List.map (fun node ->
                let count =
                    adjacencyList
                    |> Map.toList
                    |> List.sumBy (fun (_, neighbors) ->
                        if List.contains node neighbors then 1 else 0)
                node, count)
            |> Map.ofList

        let mutableInDegree = System.Collections.Generic.Dictionary(inDegree)
        let queue = System.Collections.Generic.Queue<string>()

        inDegree
        |> Map.filter (fun _ degree -> degree = 0)
        |> Map.iter (fun node _ -> queue.Enqueue(node))

        let result = ResizeArray<string>()

        while queue.Count > 0 do
            let node = queue.Dequeue()
            result.Add(node)

            match Map.tryFind node adjacencyList with
            | Some neighbors ->
                for neighbor in neighbors do
                    mutableInDegree[neighbor] <- mutableInDegree[neighbor] - 1
                    if mutableInDegree[neighbor] = 0 then
                        queue.Enqueue(neighbor)
            | None -> ()

        List.ofSeq result

    let countPaths (adjacencyList: Map<string, string list>) (topoSorted: string list) (startNode: string) (endNode: string) =
        let mutable pathCounts = Map.ofList [ startNode, 1L ]

        for node in topoSorted do
            let currentPaths = Map.tryFind node pathCounts |> Option.defaultValue 0L

            if currentPaths > 0L then
                match Map.tryFind node adjacencyList with
                | Some neighbors ->
                    for neighbor in neighbors do
                        let existingPaths = Map.tryFind neighbor pathCounts |> Option.defaultValue 0L
                        pathCounts <- Map.add neighbor (existingPaths + currentPaths) pathCounts
                | None -> ()

        Map.tryFind endNode pathCounts |> Option.defaultValue 0L

    let part1 input =

        let adjacencyList = input |> Map.ofList

        let topoSorted = kahnTopologicalSort adjacencyList

        countPaths adjacencyList topoSorted "you" "out"

    let part2 input =

        let adjacencyList = input |> Map.ofList

        let topoSorted = kahnTopologicalSort adjacencyList

        let svrToFft = countPaths adjacencyList topoSorted "svr" "fft"
        let fftToDac = countPaths adjacencyList topoSorted "fft" "dac" // order figured out manually!
        let dacToOut = countPaths adjacencyList topoSorted "dac" "out"

        svrToFft * fftToDac * dacToOut

    let puzzle =
        Puzzle.init
            11
            (getInput >> parse)
            part1 (Some 764L)
            part2 (Some 462444153119850L)
