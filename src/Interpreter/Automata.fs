module Interpreter.Automata

open System.Collections.Generic
open MatrixLib.SparseMatrixQT
open MatrixLib.AlgebraicStructures
open FiniteAutomata
open Quadtrees.MutableQT

[<Struct>]
type MatrixNFA<'t> =
    val Start: HashSet<int>
    val Final: HashSet<int>
    val Transitions: HashSet<NFASmb<'t>> SparseMatrixQT

    new(start, final, transitions) =
        { Start = start
          Final = final
          Transitions = transitions }

let inline addSets (set1: HashSet<_>) (set2: HashSet<_>) =
    let res =
        if set1 = null then HashSet<_>()
        else HashSet<_>(set1)

    res.UnionWith set2
    res

let inline multiplySets (set1: HashSet<_>) (set2: HashSet<_>) =
    let res = HashSet<_>()

    for x in set1 do
        for y in set2 do
            match x, y with
            | Eps, smth -> res.Add smth |> ignore
            | _ -> ()

    res

let semiringBool =
    { GetGenericZero = fun () -> false
      Addition = (||)
      Multiplication = (&&) }
    

let nfaToMatrixNFA (nfa: NFA<_>) =
    let resMtx =
        let maxState =
            nfa.Transitions
            |> List.fold (fun a (start, _, final) -> max (max start a) final) 0

        let mutable sparseMtx =
            SparseMatrixQT((maxState + 1), (fun () -> HashSet<_>()))

        nfa.Transitions
        |> List.iter (
            fun (s, l, f) ->
                let gotElem = sparseMtx.[s, f]
                gotElem.Add l |> ignore
                sparseMtx.[s, f] <- gotElem
            )

        sparseMtx

    MatrixNFA<_>(HashSet<_>([ nfa.Start ]), HashSet<_>([ nfa.Final ]), resMtx)

let seqToAtm (input: list<_>) =
    let resMtx =
        let mutable sparseMtx =
            SparseMatrixQT((input.Length + 1), (fun () -> HashSet<_>()))
        
        for i in 0 .. input.Length - 1 do
            let gotElem = sparseMtx.[i, i + 1]
            gotElem.Add(Smb(input.[i])) |> ignore
            sparseMtx.[i, i + 1] <- gotElem
        sparseMtx

    MatrixNFA<_>(HashSet<_>([ 0 ]), HashSet<_>([ input.Length ]), resMtx)

let toDot (nfa: MatrixNFA<_>) outFile =
    let header =
        [ "digraph nfa"
          "{"
          "rankdir = LR"
          "node [shape = circle];"
          for s in nfa.Start do
              $"%A{s}[shape = circle, label = \"%A{s}_Start\"]" ]

    let footer =
        [ for s in nfa.Final do
            $"%A{s}[shape = doublecircle]"
          "}" ]

    let transitionsDotStrings start finish (hashSet: HashSet<NFASmb<_>>) =
        seq {
            for elem in hashSet do
                let label =
                    match elem with
                    | Eps -> "Eps"
                    | Smb symbol -> symbol.ToString()

                $"{start} -> {finish} [label = \"{label}\"]\n"
        }
        |> List.ofSeq

    let mutable _content = List.empty

    let saveToList (x: float) (y: float) hashSet =
        let x, y = int x, int y
        _content <- _content @ transitionsDotStrings x y hashSet

    nfa.Transitions.quadtree
    |> MutableQT.iteri saveToList


    System.IO.File.WriteAllLines(outFile, header @ _content @ footer)


let epsClosure (atm: MatrixNFA<_>) =
    let semiringOfSets =
        { GetGenericZero = fun () -> HashSet<_>()
          Addition = addSets
          Multiplication = multiplySets }

    let epsCls =
        SparseMath.closure semiringOfSets (fun i -> i.Count > 0) atm.Transitions

    let intermediateResult =
        MatrixNFA<_>(atm.Start, atm.Final, epsCls)

    // toDot intermediateResult "eps-closure-step-1.dot"
    let newFinals = HashSet<_>()

    epsCls.quadtree
    |> MutableQT.iteri
        (fun i j x ->
            if x.Contains Eps && atm.Final.Contains(int j) then
                newFinals.Add(int i) |> ignore)

    newFinals.UnionWith atm.Final

    epsCls.quadtree
    |> MutableQT.iteri (fun _ _ x -> x.Remove Eps |> ignore)

    let res =
        MatrixNFA<_>(atm.Start, newFinals, epsCls)

    // toDot res "eps-closure-without-eps-edges.dot"

    let boolMtx =
        res.Transitions
        |> SparseMatrixQT.map (fun () -> false) (=) (fun x -> x.Count > 0)

    let reachable =
        SparseMath.closure semiringBool id boolMtx

    let reachableFromStart = HashSet<_>()

    reachable.quadtree
    |> MutableQT.iteri
        (fun i j x ->
            if x && atm.Start.Contains(int i) then
                reachableFromStart.Add(int j) |> ignore)

    reachableFromStart.UnionWith atm.Start

    let newStateToOldState = Dictionary<_, _>()

    reachableFromStart
    |> Seq.iteri (fun i x -> newStateToOldState.Add(i, x))

    let newTransitions =
        SparseMatrixQT.init
            newStateToOldState.Count
            (fun () -> HashSet<_>())
            (fun a -> a.SetEquals)
            (fun i j -> epsCls.[newStateToOldState.[i], newStateToOldState.[j]])

    let res =
        MatrixNFA<_>(
            newStateToOldState
            |> Seq.filter (fun x -> atm.Start.Contains x.Value)
            |> Seq.map (fun kvp -> kvp.Key)
            |> HashSet<_>,
            newStateToOldState
            |> Seq.filter (fun x -> newFinals.Contains x.Value)
            |> Seq.map (fun kvp -> kvp.Key)
            |> HashSet<_>,
            newTransitions
        )
        
    // save to result of closure (should a DFA) .dot file
    // toDot res "eps-closure-final-result.dot"
    res


let accept (nfa: MatrixNFA<_>) (input: list<_>) =
    let matchingStrNFA = seqToAtm input
    let semiringHashSets =
        { GetGenericZero = fun () -> HashSet<_>()
          Addition = addSets
          Multiplication = multiplySets }

    let intersection =
        SparseMath.tensorMultiply
            semiringHashSets
            matchingStrNFA.Transitions
            nfa.Transitions
            
    let newStartState =
        [ for s1 in matchingStrNFA.Start do
            for s2 in nfa.Start do
                s1 * nfa.Transitions.size + s2 ]
        |> HashSet<_>

    let newFinalStates =
        [ for s1 in matchingStrNFA.Final do
            for s2 in nfa.Final do
                s1 * nfa.Transitions.size + s2 ]

    // save results of intersection to .dot file
    // toDot (MatrixNFA<_>(newStartState, HashSet<_>(newFinalStates), intersection)) "out-intersection.dot"

    let projected =
        intersection
        |> SparseMatrixQT.map (fun () -> false) (=) (fun s -> s.Count > 0)

    let reachability =
        SparseMath.closure semiringBool id projected

    let result =
        newFinalStates
        |> List.fold (
            fun a state ->
                let rest =
                    newStartState |>
                    Seq.fold (fun a2 s2 ->
                    a2 || reachability.[s2, state]) false
                a || rest)
            false

    result

let findAll (nfa: MatrixNFA<_>) (input: list<_>) =
    let nfa2 = seqToAtm input

    let semiringOfSets =
        { GetGenericZero = fun () -> HashSet<_>()
          Addition = addSets
          Multiplication = multiplySets }

    let intersection =
        SparseMath.tensorMultiply semiringOfSets nfa2.Transitions nfa.Transitions

    let newStartState =
        [ for s1 in 0 .. nfa2.Transitions.size - 1 do
              for s2 in nfa.Start do
                  s1 * nfa.Transitions.size + s2 ]
        |> HashSet<_>

    let newFinalStates =
        [ for s1 in 0 .. nfa2.Transitions.size - 1 do
              for s2 in nfa.Final do
                  yield s1 * nfa.Transitions.size + s2 ]

    // toDot (MatrixNFA<_>(newStartState, HashSet<_>(newFinalStates), intersection)) "out-find-all.dot"

    let projected =
        intersection
        |> SparseMatrixQT.map (fun () -> false) (=) (fun s -> s.Count > 0)

    let reachability =
        SparseMath.closure semiringBool id projected

    [ for s1 in newFinalStates do
          for s2 in newStartState do
              if reachability.[s2, s1] || s1 = s2 then
                  yield (s2 / nfa.Transitions.size, s1 / nfa.Transitions.size) ]
