module Interpreter.Automata
open Interpreter.Symbols
open System.Collections.Generic
open MatrixLib.MatrixAlgebra
open MatrixLib.SparseMtx
open MatrixLib.AlgStructs
open MatrixLib.Operators.CommonOps
open MatrixLib.AlgStructs.CommonSR
open FiniteAutomata
open Quadtrees.MutableQT

let private setsEquality (set1: HashSet<_>) (set2: HashSet<_>) =
    set1.SetEquals(set2)
        
let private setsAddition (set1: HashSet<_>) (set2: HashSet<_>) =
    set1.UnionWith(set2)
    set1

// todo: refactor this
let private epsSmbSetsMultiply (set1: HashSet<NFASmb<_>>) (set2: HashSet<_>) =
    let res = HashSet<_>()

    for x in set1 do
        for y in set2 do
            match x, y with
            | Eps, smth -> res.Add smth |> ignore
            | _ -> ()

    res

let epsSmbSetSR () =
    let zero = fun _ -> HashSet<_>()
    Semiring(zero, setsEquality, setsAddition, epsSmbSetsMultiply)

let epsSmbSetOps () =
    epsSmbSetSR() |> getOps

let seqToAtm (input: list<_>) =
    let resMtx =
        let mutable sparseMtx =
            SparseMtx((input.Length + 1), epsSmbSetOps())
        
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

    let saveToList x y hashSet =
        _content <- _content @ transitionsDotStrings x y hashSet

    nfa.Transitions.tree
    |> MutableQT.iteri saveToList

    System.IO.File.WriteAllLines(outFile, header @ _content @ footer)

let epsClosure (atm: MatrixNFA<_>) =
    let epsCls =
        MatrixAlgebra.closure (epsSmbSetSR()) (fun i -> i.Count > 0) atm.Transitions

    let newFinals = HashSet<_>()

    epsCls.tree
    |> MutableQT.iteri
        (fun i j x ->
            if x.Contains Eps && atm.Final.Contains(j) then
                newFinals.Add(i) |> ignore)

    newFinals.UnionWith atm.Final

    epsCls.tree
    |> MutableQT.iteri (fun _ _ x -> x.Remove Eps |> ignore)

    let res =
        MatrixNFA<_>(atm.Start, newFinals, epsCls)

    let boolMtx =
        res.Transitions
        |> SparseMtx.map booleanOps (fun x -> x.Count > 0)

    let reachable =
        MatrixAlgebra.closure booleanSR id boolMtx

    let reachableFromStart = HashSet<_>()

    reachable.tree
    |> MutableQT.iteri
        (fun i j x ->
            if x && atm.Start.Contains(i) then
                reachableFromStart.Add(j) |> ignore)

    reachableFromStart.UnionWith atm.Start

    let newStateToOldState = Dictionary<_, _>()

    reachableFromStart
    |> Seq.iteri (fun i x -> newStateToOldState.Add(i, x))

    let newTransitions =
        SparseMtx.init
            newStateToOldState.Count
            (epsSmbSetOps())
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
        
    res

let intersect (nfaA: MatrixNFA<_>) (nfaB: MatrixNFA<_>) =
    MatrixAlgebra.kroneckerProduct
            (epsSmbSetSR())
            nfaA.Transitions
            nfaB.Transitions
    
let getProjection (intersection: SparseMtx<HashSet<NFASmb<'a>>>) =
        intersection
        |> SparseMtx.map booleanOps (fun s -> s.Count > 0)
    
let accept (nfa: MatrixNFA<_>) (input: list<_>) =
    let matchingStrNFA = seqToAtm input
    let intersection = intersect matchingStrNFA nfa
    let newStartState =
        [ for s1 in matchingStrNFA.Start do
                for s2 in nfa.Start do
                    s1 * nfa.Transitions.size + s2 ]
            |> HashSet<_>       
    let newFinalStates =
        [ for s1 in matchingStrNFA.Final do
            for s2 in nfa.Final do
                 s1 * nfa.Transitions.size + s2 ]
        
    let projected = getProjection intersection
    let reachability = MatrixAlgebra.closure booleanSR id projected

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
    let intersection = intersect nfa2 nfa

    let newStartState =
        [ for s1 in 0 .. nfa2.Transitions.size - 1 do
              for s2 in nfa.Start do
                  s1 * nfa.Transitions.size + s2 ]
        |> HashSet<_>

    let newFinalStates =
        [ for s1 in 0 .. nfa2.Transitions.size - 1 do
              for s2 in nfa.Final do
                  yield s1 * nfa.Transitions.size + s2 ]

    let projected = getProjection intersection
    let reachability = MatrixAlgebra.closure booleanSR id projected

    [ for s1 in newFinalStates do
          for s2 in newStartState do
              if reachability.[s2, s1] || s1 = s2 then
                  yield (s2 / nfa.Transitions.size, s1 / nfa.Transitions.size) ]
