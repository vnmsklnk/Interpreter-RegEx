module Interpreter.FiniteAutomata

open System.Collections.Generic
open Interpreter.Symbols
open MatrixLib
open MatrixLib.Structures
open MatrixLib.MatrixAlgebra
open MatrixLib.SparseMtx
open Quadtrees.MutableQT

let private makeSet (element: 'a) = HashSet<'a>(seq {element})
    
let private first (hashSet: HashSet<_>) = hashSet |> seq |> Seq.head

/// Adds element to HashSet taken from sparse matrix by index (i, j)
/// Returns updated hash set
let private addTo (i, j) element (matrix: SparseMtx<HashSet<_>>) =
    let updated = HashSet(matrix.[i, j])
    updated.Add(element) |> ignore
    updated

[<Struct>]
type MatrixNFA<'t> =
    val start: HashSet<int>
    val final: HashSet<int>
    val transitions: HashSet<NFASmb<'t>> SparseMtx

    new(start, final, transitions) =
        { start = start
          final = final
          transitions = transitions }
    
let private intersect matrixA matrixB =
    MatrixAlgebra.kroneckerProduct'
        Semirings.hashSetsSR<_>
        matrixA
        matrixB
    
let seqToAtm (input: list<_>) =
    let resMtx =
        let mutable sparseMtx =
            SparseMtx((input.Length + 1), Operators.hashSetOps)
        
        for i in 0 .. input.Length - 1 do
            let gotElem = sparseMtx.[i, i + 1]
            gotElem.Add(Smb(input.[i])) |> ignore
            sparseMtx.[i, i + 1] <- gotElem
        sparseMtx

    MatrixNFA<_>(HashSet<_>([0]), HashSet<_>([ input.Length ]), resMtx)
    
let epsClosure (atm: MatrixNFA<_>) =
    let epsilonClosureMultiply (setA: HashSet<NFASmb<_>>) (setB: HashSet<NFASmb<_>>) =
        let res = HashSet<_>()
        for a in setA do
            for b in setB do
                match a, b with
                | Eps, something -> res.Add something |> ignore
                | _ -> ()
        res
    
    let epsilonSetsSemiring =
        Semiring(
            (fun _ -> HashSet<_>()),
            Operators.hashSetEq,
            Semirings.hashSetAdd,
            epsilonClosureMultiply
        )
    
    let epsCls =
        MatrixAlgebra.closure epsilonSetsSemiring atm.transitions

    let newFinals = HashSet<_>()

    epsCls.tree
    |> MutableQT.iteri
        (fun i j x ->
            if x.Contains Eps && atm.final.Contains(int j) then
                newFinals.Add(int i) |> ignore)

    newFinals.UnionWith atm.final

    epsCls.tree
    |> MutableQT.iteri (fun _ _ x -> x.Remove Eps |> ignore)

    let res =
        MatrixNFA<_>(atm.start, newFinals, epsCls)

    let boolMtx =
        res.transitions
        |> SparseMtx.map Operators.booleanOps (fun x -> x.Count > 0)

    let reachable =
        MatrixAlgebra.closure Semirings.booleanSR boolMtx

    let reachableFromStart = HashSet<_>()

    reachable.tree
    |> MutableQT.iteri
        (fun i j x ->
            if x && atm.start.Contains(int i) then
                reachableFromStart.Add(int j) |> ignore)

    reachableFromStart.UnionWith atm.start

    let newStateToOldState = Dictionary<_, _>()

    reachableFromStart
    |> Seq.iteri (fun i x -> newStateToOldState.Add(i, x))

    let newTransitions =
        SparseMtx.init
            newStateToOldState.Count
            (epsilonSetsSemiring |> getOps)
            (fun i j -> epsCls.[newStateToOldState.[i], newStateToOldState.[j]])

    let res =
        MatrixNFA<_>(
            newStateToOldState
            |> Seq.filter (fun x -> atm.start.Contains x.Value)
            |> Seq.map (fun kvp -> kvp.Key)
            |> HashSet<_>,
            newStateToOldState
            |> Seq.filter (fun x -> newFinals.Contains x.Value)
            |> Seq.map (fun kvp -> kvp.Key)
            |> HashSet<_>,
            newTransitions
        )
        
    res

let toDot (nfa: MatrixNFA<_>) outFile =
    let header =
        [ "digraph nfa"
          "{"
          "rankdir = LR"
          "node [shape = circle];"
          for s in nfa.start do
              $"%A{s}[shape = circle, label = \"%A{s}_Start\"]" ]

    let footer =
        [ for s in nfa.final do
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

    nfa.transitions.tree
    |> MutableQT.iteri saveToList

    System.IO.File.WriteAllLines(outFile, header @ _content @ footer)

/// Converts regex to NFA
let regexToNFA regex: MatrixNFA<_> =
    let mutable transitions = SparseMtx(2, toOps (fun () -> HashSet<NFASmb<_>>()) Operators.hashSetEq)
    let rec _go curFreeState currRegex =
        match currRegex with
        | REps ->
            let i, j = curFreeState, curFreeState + 1
            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            
            transitions.[i, j] <- transitions |> addTo (i, j) Eps
            MatrixNFA<_>(makeSet i, makeSet j, transitions)
        
        | RSmb s ->
            let i, j = curFreeState, curFreeState + 1
            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            
            transitions.[i, j] <- transitions |> addTo (i, j) (Smb(s))
            MatrixNFA<_>(makeSet i, makeSet j, transitions)
        
        | Alt (left, right) ->
            let lAtm = _go curFreeState left
            let rAtm = _go (first lAtm.final + 1) right
            let i = first rAtm.final + 1
            let j = first rAtm.final + 2
            
            let lAtmStart, lAtmFinal = first lAtm.start, first lAtm.final
            let rAtmStart, rAtmFinal = first rAtm.start, first rAtm.final
            
            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            
            transitions.[i, lAtmStart] <- transitions |> addTo (i, lAtmStart) Eps
            transitions.[i, rAtmStart] <- transitions |> addTo (i, rAtmStart) Eps
            transitions.[lAtmFinal, j] <- transitions |> addTo (lAtmFinal, j) Eps
            transitions.[rAtmFinal, j] <- transitions |> addTo (rAtmFinal, j) Eps

            MatrixNFA<_>(makeSet i, makeSet j, transitions)

        | Seq (left, right) ->
            let lAtm = _go curFreeState left
            let rAtm = _go (first lAtm.final + 1) right
            let i = first rAtm.final + 1
            let j = first rAtm.final + 2
            
            let lAtmStart, lAtmFinal = first lAtm.start, first lAtm.final
            let rAtmStart, rAtmFinal = first rAtm.start, first rAtm.final

            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            
            transitions.[i, lAtmStart]          <- transitions |> addTo (i, lAtmStart) Eps
            transitions.[lAtmFinal, rAtmStart]  <- transitions |> addTo (lAtmFinal, rAtmStart) Eps
            transitions.[rAtmFinal, j]          <- transitions |> addTo (rAtmFinal, j) Eps
            
            MatrixNFA<_>(makeSet i, makeSet j, transitions)

        | Star re ->
            let newAtm = _go curFreeState re
            let newAtmStart, newAtmFinal =
                first newAtm.start,
                first newAtm.final
            
            let i = first newAtm.final + 1
            let j = first newAtm.final + 2
            
            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            
            transitions.[i, newAtmStart] <- transitions |> addTo (i, newAtmStart) Eps
            transitions.[newAtmFinal, j] <- transitions |> addTo (newAtmFinal, j) Eps
            transitions.[i, j] <- transitions |> addTo (i, j) Eps
            transitions.[j, i] <- transitions |> addTo (j, i) Eps
            MatrixNFA<_>(makeSet i, makeSet j, transitions)

        | Intersect (left, right) ->
            let lAtm = _go curFreeState left
            let rAtm = _go (first lAtm.final + 1) right
            let intersection = intersect lAtm.transitions rAtm.transitions
            
            let newStartState =
                let lAtmStart = lAtm.start |> first
                let rAtmStart = rAtm.start |> first
                makeSet (lAtmStart * rAtm.transitions.size + rAtmStart)

            let newFinalState =
                let lAtmFinal = lAtm.final |> first
                let rAtmFinal = rAtm.final |> first
                makeSet (lAtmFinal * rAtm.transitions.size + rAtmFinal)
                
            MatrixNFA<_>(newStartState, newFinalState, intersection)
        
    _go 0 regex

let getProjection (intersection: SparseMtx<HashSet<NFASmb<'a>>>) =
    intersection
    |> SparseMtx.map Operators.booleanOps (fun s -> s.Count > 0)
    
let accept (nfa: MatrixNFA<_>) (input: list<_>) =
    let matchingStrNFA = seqToAtm input
    let intersection = intersect matchingStrNFA.transitions nfa.transitions
    let newStartState =
        [ for s1 in matchingStrNFA.start do
                for s2 in nfa.start do
                    s1 * nfa.transitions.size + s2 ]
            |> HashSet<_>       
    let newFinalStates =
        [ for s1 in matchingStrNFA.final do
            for s2 in nfa.final do
                 s1 * nfa.transitions.size + s2 ]
        
    let projected = getProjection intersection
    let reachability = MatrixAlgebra.closure Semirings.booleanSR projected

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
    let intersection = intersect nfa2.transitions nfa.transitions

    let newStartState =
        [ for s1 in 0 .. nfa2.transitions.size - 1 do
              for s2 in nfa.start do
                  s1 * nfa.transitions.size + s2 ]
        |> HashSet<_>

    let newFinalStates =
        [ for s1 in 0 .. nfa2.transitions.size - 1 do
              for s2 in nfa.final do
                  yield s1 * nfa.transitions.size + s2 ]

    let projected = getProjection intersection
    let reachability = MatrixAlgebra.closure Semirings.booleanSR projected

    [ for s1 in newFinalStates do
          for s2 in newStartState do
              if reachability.[s2, s1] || s1 = s2 then
                  yield (s2 / nfa.transitions.size, s1 / nfa.transitions.size) ]