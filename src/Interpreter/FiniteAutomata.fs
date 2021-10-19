module Interpreter.FiniteAutomata
open Quadtrees.MutableQT
open System
open MatrixLib.AlgStructs.CommonSR
open System.Collections.Generic
open Interpreter.Symbols
open MatrixLib.AlgStructs
open MatrixLib.MatrixAlgebra
open MatrixLib.Operators
open MatrixLib.Operators.CommonOps
open MatrixLib.SparseMtx

[<Struct>]
type MatrixNFA<'t> =
    val Start: HashSet<int>
    val Final: HashSet<int>
    val Transitions: HashSet<NFASmb<'t>> SparseMtx

    new(start, final, transitions) =
        { Start = start
          Final = final
          Transitions = transitions }

let toSet (elem: 'a) = HashSet<'a>(seq {elem})
let getElemFromSet (set: HashSet<int>) = set |> seq |> Seq.head
let addNToSetElem (set: HashSet<int>) (n: int) = (getElemFromSet set) + n

let addToMtx (i, j) elem (mat: SparseMtx<HashSet<NFASmb<_>>>) =
    let newHashSet = HashSet(mat.[i, j])
    newHashSet.Add(elem) |> ignore
    newHashSet
let private setsEquality (set1: HashSet<_>) (set2: HashSet<_>) =
    set1.SetEquals(set2)
        
let private setsAddition (set1: HashSet<_>) (set2: HashSet<_>) =
    set1.UnionWith(set2)
    set1

let private epsSmbSetsMultiply (set1: HashSet<NFASmb<_>>) (set2: HashSet<_>) =
    let res = HashSet<_>()

    for x in set1 do
        for y in set2 do
            match x, y with
            | Eps, smth -> res.Add smth |> ignore
            | _ -> ()

    res
    
let multiplicationSets (s1:HashSet<_>) (s2: HashSet<_>) =
    let res = HashSet<_>(s1) in res.IntersectWith s2
    res
    
let epsSmbSetSR () =
    let zero = fun _ -> HashSet<_>()
    Semiring(zero, setsEquality, setsAddition, epsSmbSetsMultiply)
    
let epsSmbSetOps () =
    epsSmbSetSR() |> getOps
  
let standardSR () =
    let zero = fun _ -> HashSet<_>()
    Semiring(zero, setsEquality, setsAddition, multiplicationSets)
    
let intersect a b =
    MatrixAlgebra.kroneckerProduct' (standardSR()) a b
    
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
    
let epsClosure (atm: MatrixNFA<_>) =
    let epsCls =
        MatrixAlgebra.closure (epsSmbSetSR()) atm.Transitions

    let newFinals = HashSet<_>()

    epsCls.tree
    |> MutableQT.iteri
        (fun i j x ->
            if x.Contains Eps && atm.Final.Contains(int j) then
                newFinals.Add(int i) |> ignore)

    newFinals.UnionWith atm.Final

    epsCls.tree
    |> MutableQT.iteri (fun _ _ x -> x.Remove Eps |> ignore)

    let res =
        MatrixNFA<_>(atm.Start, newFinals, epsCls)

    let boolMtx =
        res.Transitions
        |> SparseMtx.map booleanOps (fun x -> x.Count > 0)

    let reachable =
        MatrixAlgebra.closure CommonSR.booleanSR boolMtx

    let reachableFromStart = HashSet<_>()

    reachable.tree
    |> MutableQT.iteri
        (fun i j x ->
            if x && atm.Start.Contains(int i) then
                reachableFromStart.Add(int j) |> ignore)

    reachableFromStart.UnionWith atm.Start

    let newStateToOldState = Dictionary<_, _>()

    reachableFromStart
    |> Seq.iteri (fun i x -> newStateToOldState.Add(i, x))

    let newTransitions =
        SparseMtx.init
            newStateToOldState.Count
            (epsSmbSetSR() |> getOps)
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

/// Converts regex to NFA
let regexToNFA regex: MatrixNFA<_> =
    let mutable transitions = SparseMtx(2, toOps (fun () -> HashSet<NFASmb<_>>()) CommonOps.hashSetEq)
    let rec _go curFreeState currRegex =
        match currRegex with
        | REps ->
            let i, j = curFreeState, curFreeState + 1
            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            transitions.[i, j] <- transitions |> addToMtx (i, j) Eps
            MatrixNFA<_>(toSet i, toSet j, transitions)
        | RSmb s ->
            let i, j = curFreeState, curFreeState + 1
            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            transitions.[i, j] <- transitions |> addToMtx (i, j) (Smb(s))
            MatrixNFA<_>(toSet i, toSet j, transitions)
        | Alt (left, right) ->
            let lAtm = _go curFreeState left
            let rAtm = _go (addNToSetElem lAtm.Final 1) right
            let i = addNToSetElem rAtm.Final 1
            let j = addNToSetElem rAtm.Final 2
            
            let lAtmStart, lAtmFinal = getElemFromSet lAtm.Start, getElemFromSet lAtm.Final
            let rAtmStart, rAtmFinal = getElemFromSet rAtm.Start, getElemFromSet rAtm.Final
            
            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            
            transitions.[i,  lAtmStart] <-  transitions |> addToMtx (i,  lAtmStart) Eps
            transitions.[i,  rAtmStart] <-  transitions |> addToMtx (i,  rAtmStart) Eps
            transitions.[lAtmFinal, j] <-  transitions |> addToMtx (lAtmFinal, j) Eps
            transitions.[rAtmFinal, j] <-  transitions |> addToMtx (rAtmFinal, j) Eps

            MatrixNFA<_>(toSet i, toSet j, transitions)

        | Seq (left, right) ->
            let lAtm = _go curFreeState left
            let rAtm = _go (addNToSetElem lAtm.Final 1) right
            let i = addNToSetElem rAtm.Final 1
            let j = addNToSetElem rAtm.Final 2
            
            let lAtmStart, lAtmFinal = getElemFromSet lAtm.Start, getElemFromSet lAtm.Final
            let rAtmStart, rAtmFinal = getElemFromSet rAtm.Start, getElemFromSet rAtm.Final

            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            
            transitions.[i, lAtmStart]          <- transitions |> addToMtx (i, lAtmStart) Eps
            transitions.[lAtmFinal, rAtmStart]  <- transitions |> addToMtx (lAtmFinal, rAtmStart) Eps
            transitions.[rAtmFinal, j]          <- transitions |> addToMtx (rAtmFinal, j) Eps
            
            MatrixNFA<_>(toSet i, toSet j, transitions)

        | Star re ->
            let newAtm = _go curFreeState re
            let newAtmStart, newAtmFinal =
                getElemFromSet newAtm.Start,
                getElemFromSet newAtm.Final
            
            let i = addNToSetElem newAtm.Final 1
            let j = addNToSetElem newAtm.Final 2
            
            if j >= transitions.size then
                transitions <- transitions |> SparseMtx.doubleSize
            
            transitions.[i,    newAtmStart] <- transitions |> addToMtx (i,    newAtmStart) Eps
            transitions.[newAtmFinal, j] <- transitions |> addToMtx (newAtmFinal, j) Eps
            transitions.[i,    j] <- transitions |> addToMtx (i,    j) Eps
            transitions.[j,    i] <- transitions |> addToMtx (j,    i) Eps
            MatrixNFA<_>(toSet i, toSet j, transitions)

        | Intersect (left, right) ->
            let lAtm = _go curFreeState left
            let rAtm = _go (addNToSetElem lAtm.Final 1) right

            let intersection = intersect lAtm.Transitions rAtm.Transitions
            let intersectionDebug = intersection |> SparseMtx.toArray2D
            
            let i = 20
            let debug = intersectionDebug.[i, 0..intersection.size]
            printfn $"Intersection arr 2D %A{debug}"
            //printfn "intersex = %A" (intersection |> SparseMtx.toArray2D)
            
            let newStartState =
                let lAtmStart = lAtm.Start |> getElemFromSet
                let rAtmStart = rAtm.Start |> getElemFromSet
                toSet (lAtmStart * rAtm.Transitions.size + rAtmStart)

            let newFinalState =
                let lAtmFinal = lAtm.Final |> getElemFromSet
                let rAtmFinal = rAtm.Final |> getElemFromSet
                toSet (lAtmFinal * rAtm.Transitions.size + rAtmFinal)
                
            MatrixNFA<_>(newStartState, newFinalState, intersection)
        
    _go 0 regex

let getProjection (intersection: SparseMtx<HashSet<NFASmb<'a>>>) =
    intersection
    |> SparseMtx.map booleanOps (fun s -> s.Count > 0)
    
let accept (nfa: MatrixNFA<_>) (input: list<_>) =
    let matchingStrNFA = seqToAtm input
    let intersection = intersect matchingStrNFA.Transitions nfa.Transitions
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
    let reachability = MatrixAlgebra.closure booleanSR projected

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
    let intersection = intersect nfa2.Transitions nfa.Transitions

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
    let reachability = MatrixAlgebra.closure booleanSR projected

    [ for s1 in newFinalStates do
          for s2 in newStartState do
              if reachability.[s2, s1] || s1 = s2 then
                  yield (s2 / nfa.Transitions.size, s1 / nfa.Transitions.size) ]