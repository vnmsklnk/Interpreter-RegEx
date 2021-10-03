module Interpreter.AstToDot

type BinTree<'a> =
    | Tree of 'a * BinTree<'a> * BinTree<'a>
    | Empty

[<AutoOpen>]
module BinTree =
    let single value =
        Tree(value, Empty, Empty)

    let collectASTsToDot (expressionTrees: list<BinTree<string>>) =
        let collectTree label (start, nodes, trans) (tree: BinTree<string>) =
            let mutable current = start

            let visit =
                fun (nd: list<_>, tr: list<_>) rootElem ->
                    current <- current + 1
                    (nd @ [ $"\tx{current}" + rootElem ], tr)

            let rec loop (nodes: list<string>, trans: list<string>) tree =
                match tree with
                | Empty -> (nodes, trans)
                | Tree (x, Empty, Empty) -> visit (nodes, trans) x
                | Tree (x, left, Empty) ->
                    let nd, tr = visit (nodes, trans) x
                    let rootNum = current
                    let nodesL, transL = loop (nd, tr) left
                    nodesL,
                    (transL
                     @ [ $"\t\"%s{label}{rootNum}\" -> \"%s{label}{rootNum + 1}\";\n" ])

                | Tree (x, Empty, right) ->
                    let nd, tr = visit (nodes, trans) x
                    let rootNum = current
                    let nodesR, transR = loop (nd, tr) right
                    nodesR,
                    (transR
                     @ [ $"\t\"%s{label}{rootNum}\" -> \"%s{label}{rootNum + 1}\";\n" ])

                | Tree (x, left, right) ->
                    let nd, tr = visit (nodes, trans) x
                    let rootNum = current
                    let nodesL, transL = loop (nd, tr) left
                    let leftLastNum = current
                    let nodesR, transR = loop (nodesL, transL) right
                    let trL = $"\t\"%s{label}{rootNum}\" -> \"%s{label}{rootNum + 1}\";\n"
                    let trR = $"\t\"%s{label}{rootNum}\" -> \"%s{label}{leftLastNum + 1}\";\n"
                    nodesR, (transR @ [ trL; trR ])

            let acc = nodes, trans
            let nodes', trans' = loop acc tree
            current, nodes', trans'

        let mutable lastCounter = 0
        let mutable nodes, trans = [ "" ], [ "" ]

        for tree in expressionTrees do
            let _last, _nodes, _trans =
                collectTree "x" (lastCounter, nodes, trans) tree

            lastCounter <- _last
            nodes <- _nodes
            trans <- _trans

        nodes, trans
    
let makeLabel label =
    $" [shape = ellipse, label = \"%s{label}\"];\n"

let rec procRegExp expr =
    match expr with
    | AST.RSmb symbol -> single (makeLabel (string symbol))
    | AST.RVar (AST.Var callingName) ->
        let varCall = makeLabel "Regex.RVar"
        let expr = makeLabel callingName
        Tree(varCall, single expr, Empty)
    | AST.Opt regex ->
        let opt = makeLabel "Regex.Opt"
        Tree(opt, (procRegExp regex), Empty)
    | AST.Star regex ->
        let star = makeLabel "Regex.Star"
        Tree(star, (procRegExp regex), Empty)
    | AST.Seq (regex1, regex2) ->
        let seq = makeLabel "Regex.Seq"
        let first, second = procRegExp regex1, procRegExp regex2
        Tree(seq, first, second)
    | AST.Alt (regex1, regex2) ->
        let alt = makeLabel "Regex.Alt"
        let first, second = procRegExp regex1, procRegExp regex2
        Tree(alt, first, second)
    | AST.Intersect (regex1, regex2) ->
        let intersect = makeLabel "Regex.Intersect"
        let first, second = procRegExp regex1, procRegExp regex2
        Tree(intersect, first, second)

let rec procStatement stmt =
    match stmt with
    | AST.PrintToDot (AST.Var name, pathString) ->
        let varCall = makeLabel name
        Tree(makeLabel "PrintToDot", single varCall, single (makeLabel pathString))
    | AST.Print (AST.Var name) ->
        let varCall = makeLabel name
        Tree(makeLabel "Print", single varCall, Empty)
    | AST.VDecl (AST.Var name, expr) ->
        let decl = makeLabel "VDecl"
        let name = single (makeLabel name)
        match expr with
        | AST.FindAll (str, exp) ->
            let findAll = makeLabel "AST.FindAll"
            let strLabeled = makeLabel str
            let expression = procRegExp exp
            Tree(decl, name, Tree(findAll, single strLabeled, expression))
        | AST.IsAcceptable (str, exp) ->
            let isAcceptable = makeLabel "AST.IsAcceptable"
            let strLabeled = makeLabel str
            let expression = procRegExp exp
            Tree(decl, name, Tree(isAcceptable, single strLabeled, expression))
        | AST.RegExp regexp -> Tree(decl, name, procRegExp regexp)

let astToDot output (programAST: AST.Program) =
    let rec statementsAsBinTrees accList statements =
        match statements with
        | [] -> accList
        | stmt :: tail ->
            let treeOfStmt = procStatement stmt
            statementsAsBinTrees (accList @ [ treeOfStmt ]) tail

    let nodes, trans =
        programAST
        |> statementsAsBinTrees []
        |> collectASTsToDot

    let body =
        System.String.Concat(nodes)
        + System.String.Concat(trans)

    System.IO.File.WriteAllText(output, $"digraph G {{\n{body}}}")