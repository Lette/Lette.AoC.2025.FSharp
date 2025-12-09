namespace Lette.AoC2025

module Program =

    let parseArgs args =
        let args' = args |> List.ofArray

        let rec loop args acc =
            match args with
            | [] -> acc

            | "-l"                        :: xs -> loop xs (acc |> Settings.withLatest)
            | "--latest"                  :: xs -> loop xs (acc |> Settings.withLatest)

            | "-a"                        :: xs -> loop xs (acc |> Settings.withAll)
            | "--all"                     :: xs -> loop xs (acc |> Settings.withAll)

            | "-d"                        :: [] -> failwith "-d must be followed by a valid day"
            | "-d"       :: IsInteger day :: xs -> loop xs (acc |> Settings.withDay day)
            | "-d"       :: bad           :: _  -> failwith $"'%s{bad}' is an unknown day"
            | "--day"                     :: [] -> failwith "--day must be followed by a valid day"
            | "--day"    :: IsInteger day :: xs -> loop xs (acc |> Settings.withDay day)
            | "--day"    :: bad           :: _  -> failwith $"'%s{bad}' is an unknown day"

            | "-e"                        :: [] -> failwith "-e must be followed by a valid day"
            | "-e"       :: IsInteger day :: xs -> loop xs (acc |> Settings.withoutDay day)
            | "-e"       :: bad           :: _  -> failwith $"'%s{bad}' is an unknown day"
            | "--except"                  :: [] -> failwith "--except must be followed by a valid day"
            | "--except" :: IsInteger day :: xs -> loop xs (acc |> Settings.withoutDay day)
            | "--except" :: bad           :: _  -> failwith $"'%s{bad}' is an unknown day"

            | "-p"                        :: [] -> failwith "-p must be followed by a valid part (1 or 2)"
            | "-p"       :: "1"           :: xs -> loop xs (acc |> Settings.withPart First)
            | "-p"       :: "2"           :: xs -> loop xs (acc |> Settings.withPart Second)
            | "-p"       :: bad           :: _  -> failwith $"'%s{bad}' is an unknown part"
            | "--part"                    :: [] -> failwith "--part must be followed by a valid part (1 or 2)"
            | "--part"   :: "1"           :: xs -> loop xs (acc |> Settings.withPart First)
            | "--part"   :: "2"           :: xs -> loop xs (acc |> Settings.withPart Second)
            | "--part"   :: bad           :: _  -> failwith $"'%s{bad}' is an unknown part"

            | "-t"                        :: xs -> loop xs (acc |> Settings.withParserTime)
            | "--show-parser-time"        :: xs -> loop xs (acc |> Settings.withParserTime)
            | "-tx"                       :: xs -> loop xs (acc |> Settings.withoutParserTime)
            | "--hide-parser-time"        :: xs -> loop xs (acc |> Settings.withoutParserTime)

            | s                           :: _  -> failwith $"'%s{s}' is an unknown argument"

        loop args' Settings.defaults

    [<EntryPoint>]
    let main args =

        let settings = parseArgs args

        Timing.displayTimerProperties ()

        let puzzles =
            [
                Day01.puzzle
                Day02.puzzle
                Day03.puzzle
                Day04.puzzle
                Day05.puzzle
                Day06.puzzle
                Day07.puzzle
                Day08.puzzle
                Day09.puzzle
            ] : IPuzzle list

        Presentation.printHeader ()

        Puzzle.run puzzles settings
        |> Seq.tap Presentation.printResult
        |> Seq.where Puzzle.isUnsuccessful
        |> Seq.length
