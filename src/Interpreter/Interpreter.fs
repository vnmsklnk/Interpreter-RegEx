module Interpreter.Interpreter
open Interpreter.Symbols
open System.Collections.Generic
open System.IO
open Interpreter.FiniteAutomata
open FSharp.Text.Lexing
open Interpreter.Automata
open MatrixLib.SparseMtx

let private newDataToConsole = Event<string>()

let Printed = newDataToConsole.Publish

let private runtimeException = Event<string>()

let NewRuntimeException = runtimeException.Publish


type VType =
    | RE of RegEx<char>
    | Bool of bool
    | Lst of list<int * int>

    member this.ToString =
        match this with
        | RE x -> x.ToString()
        | Bool x -> x.ToString()
        | Lst x -> x.ToString()

let rec processRegExp (valueDict: Dictionary<_, _>) re =
    match re with
    | AST.RVar v ->
        let data =
            try
                valueDict.[v]
            with
            | _ ->
                $"Variable %A{v} is not declared."
                |> runtimeException.Trigger

                failwith $"Variable %A{v} is not declared."

        match data with
        | RE r -> r
        | Bool _ ->
            $"Variable %A{v} has type bool, but regexp is expected."
            |> runtimeException.Trigger

            failwithf $"Variable %A{v} has type bool, but regexp is expected."
        | Lst _ ->
            $"Variable %A{v} has type list, but regexp is expected."
            |> runtimeException.Trigger

            failwithf $"Variable %A{v} has type list, but regexp is expected."

    | AST.RSmb smb -> RegEx.RSmb smb
    | AST.Alt (l, r) ->
        let left = processRegExp valueDict l
        let right = processRegExp valueDict r
        RegEx.Alt(left, right)
    | AST.Seq (l, r) ->
        let l = processRegExp valueDict l
        let r = processRegExp valueDict r
        RegEx.Seq(l, r)
    | AST.Star r ->
        let r = processRegExp valueDict r
        RegEx.Star r
    | AST.Opt r ->
        let r = processRegExp valueDict r
        RegEx.Alt(RegEx.REps, r)
    | AST.Intersect (l, r) ->
        let l = processRegExp valueDict l
        let r = processRegExp valueDict r
        RegEx.Intersect(l, r)

let processExpression vDict expression =
    let makeAtm astRegex =
        let regex = processRegExp vDict astRegex
        let transitionsMtx = regexToNFA regex
        epsClosure transitionsMtx

    match expression with
    | AST.FindAll (str, re) -> Lst(findAll (makeAtm re) (str.ToCharArray() |> List.ofArray))
    | AST.IsAcceptable (str, re) -> Bool(accept (makeAtm re) (str.ToCharArray() |> List.ofArray))
    | AST.RegExp re -> RE(processRegExp vDict re)

let processStmt (vDict: Dictionary<_, _>) (pDict: Dictionary<string, string>) stmt =
    match stmt with
    | AST.Print value ->
        let varData =
            try
                vDict.[value]
            with
            | _ ->
                $"Variable %A{value} is not declared."
                |> runtimeException.Trigger

                failwithf $"Variable %A{value} is not declared."

        match varData with
        | RE reVal -> pDict.["print"] <- (pDict.["print"] + reVal.ToString() + "\n")
        | Bool boolVal -> pDict.["print"] <- (pDict.["print"] + boolVal.ToString() + "\n")
        | Lst lValues -> pDict.["print"] <- (pDict.["print"] + lValues.ToString() + "\n")
    | AST.VDecl (value, expr) ->
        if vDict.ContainsKey value then
            vDict.[value] <- processExpression vDict expr
        else
            vDict.Add(value, processExpression vDict expr)
    | AST.PrintToDot (var, path) ->
        let processVar reVar fullPath =
            match reVar with
            | RE regEx ->
                let mtxNFA =
                    regEx
                    |> regexToNFA
                match File.Exists fullPath with
                | false -> Error $"Specified file not found: {fullPath}"
                | true ->
                    toDot mtxNFA fullPath
                    Ok $"printToDot: written to {fullPath}"
            | _ as other -> Error $"Error: 'printToDot' 1 arg. should be a var with regex; instead got: %A{other}"
        
        match (processVar vDict.[var] path) with
        | Ok msg -> printfn $"%A{msg}" 
        | Error msg ->
            let _msg = $"Processing statement error: {msg}"
            msg |> runtimeException.Trigger // generate event
            failwith _msg

    vDict, pDict

let run statements =
    let vDict = Dictionary<_, _>()
    let pDict = Dictionary<_, _>()
    let varDict = Dictionary<_, _>()
    pDict.Add("print", "")

    let vD, _pD =
        List.fold (fun (d1, d2) -> processStmt d1 d2) (vDict, pDict) statements

    vD, varDict, pDict

let parseProgramToAST progText =
    let lexBuffer = LexBuffer<char>.FromString progText
    let parsed = Parser.start Lexer.tokenStream lexBuffer
    parsed