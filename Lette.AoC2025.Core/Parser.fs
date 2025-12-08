namespace Lette.AoC2025

open FParsec

[<RequireQualifiedAccess>]
module Parser =

    let run<'output> (parser: Parser<'output, unit>) input =
        match run parser input with
        | Success (result, _, _) -> result
        | Failure (msg, _, _)    -> failwith msg

namespace FParsec

[<AutoOpen>]
module Primitives =

    open Lette.AoC2025.Core

    let spaceP<'a> = pchar ' ' : Parser<_, 'a>
    let newlineP<'a> = pchar '\n' : Parser<_, 'a>

    let sepBy' p sep = p .>>. (many (attempt (sep >>. p))) |>> cons'
    let rowsOf p = sepBy' p newlineP

    let restOfLine1<'a> = many1Satisfy ((<>) '\n') <??> "at least one character before the end of the line" : Parser<string, 'a>

    let normalizeT ((a, b), c) = (a, b, c)

    let (||>>) p f = p |>> fun p -> p ||> f
