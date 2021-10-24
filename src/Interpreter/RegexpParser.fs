module Interpreter.RegexpParser

open FSharp.Text.Lexing

/// Run lexer and parser on specified string containing regex
let parseRegexFromString str =
    let lexBuffer = LexBuffer<char>.FromString str

    let regex =
        lexBuffer |> Parser.start Lexer.tokenStream

    regex
