namespace Lette.AoC2025

open System.Collections.Generic

[<AutoOpen>]
module Core =

    let flip f a b = f b a
    let dup f x = f x x

    let cons x xs = x :: xs
    let cons' (x, xs) = cons x xs

    let invoke f = f ()

    let dump x = printfn $"%A{x}"

    let countDistinctItems xss =

        let updateCounts items map =

            let updater =
                Option.map ((+) 1) >> Option.orElse (Some 1)

            let updateCount item =
                Map.change item updater

            let rec update items acc =
                match items with
                | []      -> acc
                | x :: xs -> update xs (updateCount x acc)

            update items map

        let folder map items =
            updateCounts (List.distinct items) map

        xss
        |> List.fold folder Map.empty
        |> Map.toList

    let findCommonItem xss =

        xss
        |> countDistinctItems
        |> List.where (snd >> (=) (List.length xss))
        |> List.head
        |> fst

    let (|IsInteger|_|) (s: string) =
        match System.Int32.TryParse s with
        | false, _      -> None
        | true , result -> Some result

    let gcd a b =
        let rec gcd' a b =
            if b = 0L then
                a
            else
                gcd' b (a % b)
        gcd' (abs a) (abs b)

    let lcm a b = (a * b) / (gcd a b)

    let manhattanDistance (x1, y1) (x2, y2) =
        abs (x1 - x2) + abs (y1 - y2)

    // create all combinations of length n, with repetition, from a set of items
    let rec allCombinations n items =
        match n with
        | 0 -> seq { yield [] }
        | _ -> items |> Seq.collect (fun item -> allCombinations (n - 1) items |> Seq.map (fun items -> item :: items))

    let memoize f =
        let cache = Dictionary<_, _>()
        fun x ->
            match cache.TryGetValue(x) with
            | true, res -> res
            | _ ->
                let result = f x
                cache.Add(x, result)
                result

    let numberOfDigits = float >> log10 >> int >> (+) 1
    let numberOfDigitsL: (int64 -> int) = float >> log10 >> int >> (+) 1
    let numberOfDigitsLS: (int64 -> int) = string >> _.Length

    let (%+) a b = ((a % b) + b) % b   // Always returns a value in [0..b-1]
    let (%+!) a b = (a + b) % b        // Shortcut to save one operation. Only use if a >= -b.

    let isEven x = x % 2 = 0
    let isOdd x = x % 2 = 1

    let getDivisors n =
        let rec loop d acc =
            if d * d > n then
                acc
            else if n % d = 0 then
                let acc' = d :: acc
                if d * d = n then
                    loop (d + 1) acc'
                else
                    loop (d + 1) ((n / d) :: acc')
            else
                loop (d + 1) acc
        loop 1 [] |> List.sort

    let getProperDivisors n =
        getDivisors n |> fun l -> List.take (List.length l - 1) l

[<RequireQualifiedAccess>]
module List =

    let tap f xs = List.map (fun x -> f x; x) xs

    let foldi<'t, 'state> (folder: 'state -> int -> 't -> 'state) (state: 'state) (list: 't list) =

        let rec loop index acc = function
            | []      -> acc
            | x :: xs -> loop (index + 1) (folder acc index x) xs

        loop 0 state list

    let pairs list =
        let rec loop acc = function
            | [] -> acc
            | x :: xs ->
                let pairs' = xs |> List.map (fun y -> (x, y))
                loop (pairs' @ acc) xs
        loop [] list

    let foldBack' folder =
        flip (List.foldBack (flip folder))

[<RequireQualifiedAccess>]
module Seq =

    let tap f xs = Seq.map (fun x -> f x; x) xs

[<RequireQualifiedAccess>]
module Map =

    let findOrDefault defaultValue key map =
        match Map.tryFind key map with
        | Some value -> value
        | None       -> defaultValue

[<RequireQualifiedAccess>]
module Tuple =

    let map fa fb (a, b) = (fa a, fb b)
    let map1 f (a, b) = (f a, f b)

[<RequireQualifiedAccess>]
module Array =
    let tap f xs = Array.map (fun x -> f x; x) xs

[<RequireQualifiedAccess>]
module Array2D =

    let foldi<'t, 'state> (folder: 'state -> int -> int -> 't -> 'state) (state: 'state) (array: 't[,]) =

        let sizeX = Array2D.length1 array
        let sizeY = Array2D.length2 array

        let nextIndex (x, y) =
            let y' = y + 1
            if y' = sizeY then (x + 1, 0) else (x, y')

        let rec loop (x, y) acc =
            let v = array[x, y]
            let acc' = folder acc x y v
            let x', y' = nextIndex (x, y)
            if x' = sizeX then
                acc'
            else
                loop (x', y') acc'

        loop (0, 0) state

    let find item arr =
        let maxX = arr |> Array2D.length1
        let maxY = arr |> Array2D.length2

        let rec find' x y =
            match x, y with
            | _, y when y = maxY -> find' (x + 1) 0
            | x, _ when x = maxX -> failwith "could not find item"
            | _                  -> if arr[x, y] = item then (x, y) else find' x (y + 1)

        find' 0 0

    let tryFind item arr =
        let maxX = arr |> Array2D.length1
        let maxY = arr |> Array2D.length2

        let rec find' x y =
            match x, y with
            | _, y when y = maxY -> find' (x + 1) 0
            | x, _ when x = maxX -> None // failwith "could not find item"
            | _                  -> if arr[x, y] = item then Some (x, y) else find' x (y + 1)

        find' 0 0

    let count item arr =
        let maxX = arr |> Array2D.length1
        let maxY = arr |> Array2D.length2

        let rec find' x y acc =
            match x, y with
            | _, y when y = maxY -> find' (x + 1) 0 acc
            | x, _ when x = maxX -> acc
            | _                  -> find' x (y + 1) (acc + if arr[x, y] = item then 1 else 0)

        find' 0 0 0

    let mapiRows f arr =
        let height = arr |> Array2D.length1

        [
            for x in 0 .. height - 1 do
                yield f x arr[x, *]
        ]

    let mapRows f arr =
        let height = arr |> Array2D.length1

        [
            for x in 0 .. height - 1 do
                yield f arr[x, *]
        ]

    let toListOfLists arr =
        arr
        |> mapRows Array.toList

[<RequireQualifiedAccess>]
module String =
    let indexOfAny chars (str : string) = str.IndexOfAny chars
    let lastIndexOfAny chars (str : string) = str.LastIndexOfAny chars
    let getChar index (str : string) = str[index]
    let replace (a : string) (b : string) (str : string) = str.Replace (a, b)
    let toCharArray (str : string) = str.ToCharArray ()
    let ofArray (chars : char[]) = System.String chars
    let ofList (chars : char list) : string = System.String (chars |> List.toArray)
    let regexSplit pattern input = System.Text.RegularExpressions.Regex.Split(input, pattern)
