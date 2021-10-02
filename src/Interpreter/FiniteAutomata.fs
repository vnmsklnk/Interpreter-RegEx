module Interpreter.FiniteAutomata

open System
open System.Collections.Generic

type NFASmb<'t> =
    | Eps
    | Smb of 't

/// Generic regex grammar
type RegEx<'t> =
    | REps
    | RSmb of 't
    | Seq of RegEx<'t> * RegEx<'t>
    | Alt of RegEx<'t> * RegEx<'t>
    | Star of RegEx<'t>
    | Intersect of RegEx<'t> * RegEx<'t> // todo: implement

/// Represents Non-Deterministic Finite Automata
[<Struct>]
type NFA<'t> =
    val Start: int
    val Final: int
    val Transitions: list<int * NFASmb<'t> * int>

    new(start, final, transitions) =
        { Start = start
          Final = final
          Transitions = transitions }

/// Matches string to regex grammar
let recognizeNFA (atm: NFA<_>) (input: list<_>) =
    let visited = HashSet<_>()

    let step currState currInput =
        visited.Add((currState, currInput)) |> ignore

        atm.Transitions
        |> List.choose
            (fun (start, transitionSmb, final) ->
                if start = currState then
                    match transitionSmb with
                    | Eps -> Some(final, currInput)
                    | Smb smb ->
                        match currInput with
                        | _ -> None
                        | currSmb :: rest when smb = currSmb -> Some(final, rest)
                else None)

    let rec _go configurations =
        match configurations with
        | [] -> false
        | (state, input) :: tail ->
            let containsFinal =
                configurations
                |> List.exists
                    (fun (state, input) ->
                        state = atm.Final && input = [])

            let notVisited =
                step state input
                |> List.filter
                    (fun x ->
                        visited.Contains x |> not)

            containsFinal || (_go (tail @ notVisited))

    _go [ (atm.Start, input) ]

/// Converts NFA to .dot format (graphviz),
/// saves to specified path 
let nfaToDot outputFile (nfa: NFA<'t>) =
    let header =
        [ "digraph nfa {"
          "rankdir = LR"
          "node [shape = circle];"
          $"%d{nfa.Start} [shape = circle, label = \"%d{nfa.Start}_Start\"]" ]

    let footer =
        [ $"%d{nfa.Final} [shape = doublecircle]"
          "}" ]

    let content =
        [ for start, transitionSmb, final in nfa.Transitions ->
            let label =
                match transitionSmb with
                | Eps -> "Eps"
                | Smb t -> $"%A{t}"
            $"%i{start} -> %i{final} [label = \"%s{label}\"]"]

    System.IO.File.WriteAllLines(outputFile, header @ content @ footer)

/// Converts regex to NFA
let regexToNFA regex =
    let rec _go curFreeState currRegex =
        match currRegex with
        | REps -> NFA<_>(curFreeState, curFreeState + 1, [ (curFreeState, Eps, curFreeState + 1) ])
        | RSmb s -> NFA<_>(curFreeState, curFreeState + 1, [ (curFreeState, Smb(s), curFreeState + 1) ])
        | Alt (left, right) ->
            let lAtm = _go curFreeState left
            let rAtm = _go (lAtm.Final + 1) right
            let newStart = rAtm.Final + 1
            let newFinal = rAtm.Final + 2

            let transitions =
                [ (newStart, Eps, lAtm.Start)
                  (newStart, Eps, rAtm.Start)
                  (lAtm.Final, Eps, newFinal)
                  (rAtm.Final, Eps, newFinal) ]
                @ rAtm.Transitions @ lAtm.Transitions

            NFA<_>(newStart, newFinal, transitions)

        | Seq (left, right) ->
            let lAtm = _go curFreeState left
            let rAtm = _go (lAtm.Final + 1) right
            let newStart = rAtm.Final + 1
            let newFinal = rAtm.Final + 2

            let transitions =
                [ (newStart, Eps, lAtm.Start)
                  (lAtm.Final, Eps, rAtm.Start)
                  (rAtm.Final, Eps, newFinal) ]
                @ rAtm.Transitions @ lAtm.Transitions

            NFA<_>(newStart, newFinal, transitions)

        | Star re ->
            let newAtm = _go curFreeState re
            let newStart = newAtm.Final + 1
            let newFinal = newAtm.Final + 2

            let transitions =
                [ (newStart, Eps, newAtm.Start)
                  (newAtm.Final, Eps, newFinal)
                  (newStart, Eps, newFinal)
                  (newFinal, Eps, newStart) ]
                @ newAtm.Transitions

            NFA<_>(newStart, newFinal, transitions)

        | Intersect (left, right) ->
            raise (NotImplementedException())
    _go 0 regex