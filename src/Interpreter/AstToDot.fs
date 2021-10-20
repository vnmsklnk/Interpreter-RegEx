module Interpreter.AstToDot

let makeLabel labelId id label =
    $"%s{labelId}%d{id} [shape = ellipse, label = \"%s{label}\"];\n"

let makeTransition label fromId toId =
    $"\t\"%s{label}%d{fromId}\" -> \"%s{label}%d{toId}\";\n"
        
let astToDot outPath (program: AST.Program) =
    let rec processRE (parent: int, currentExpId: int) expr: string =
        match expr with
        | AST.RSmb symbol -> makeLabel "x" currentExpId (string symbol)
        | AST.RVar (AST.Var callingName) ->
            let varCall = makeLabel "x" currentExpId "Regex.RVar"
            let varName = makeLabel "x" (currentExpId + 1) callingName
            let transL = makeTransition "x" parent currentExpId
            let transR = makeTransition "x" parent (currentExpId + 1)
            String.concat "\n" [varCall; varName; transL; transR]
        | AST.Opt regex ->
            let optLabel = makeLabel "x" currentExpId "Regex.Opt"
            let toThis = makeTransition "x" parent currentExpId
            let next = processRE (currentExpId, currentExpId + 1) regex
            String.concat "\n" [optLabel; toThis; next]
        | AST.Star regex ->
            let starLabel = makeLabel "x" currentExpId "Regex.Star"
            let toThis = makeTransition "x" parent currentExpId
            let next = processRE (currentExpId, currentExpId + 1) regex
            String.concat "\n" [starLabel; toThis; next]
        | AST.Seq (regex1, regex2) ->
            let seqLabel = makeLabel "x" currentExpId "Regex.Seq"
            let toThis = makeTransition "x" parent currentExpId
            let first, second =
                processRE (currentExpId, currentExpId + 1) regex1,
                processRE (currentExpId, currentExpId + 2) regex2
            let toLeft = makeTransition "x" currentExpId (currentExpId + 1)
            let toRight = makeTransition "x" currentExpId (currentExpId + 2)
            String.concat "\n" [seqLabel; toThis; first; second; toLeft; toRight]
            
        | AST.Alt (regex1, regex2) ->
            let alt = makeLabel "x" currentExpId  "Regex.Alt"
            let toThis = makeTransition "x" parent currentExpId
            let first, second =
                processRE (currentExpId, currentExpId + 1) regex1,
                processRE (currentExpId, currentExpId + 2) regex2
            let toLeft = makeTransition "x" currentExpId (currentExpId + 1)
            let toRight = makeTransition "x" currentExpId (currentExpId + 2)
            String.concat "\n" [alt; toThis; first; second; toLeft; toRight]
        | AST.Intersect (regex1, regex2) ->
            let intersect = makeLabel "x" currentExpId  "Regex.Intersect"
            let toThis = makeTransition "x" parent currentExpId
            let first, second =
                processRE (currentExpId, currentExpId + 1) regex1,
                processRE (currentExpId, currentExpId + 2) regex2
            let toLeft = makeTransition "x" currentExpId (currentExpId + 1)
            let toRight = makeTransition "x" currentExpId (currentExpId + 2)
            String.concat "\n" [intersect; toThis; first; second; toLeft; toRight]

    let rec procStatement (parent: int, currId: int) (stmt: AST.Stmt) =
        match stmt with
        | AST.PrintToDot (AST.Var name, pathString) ->
            let printToDotStr = makeLabel "x" currId "PrintToDot"
            let varCall = makeLabel "x" (currId + 1) name
            let pathStr = makeLabel "x" (currId + 2) pathString
            let trans1 = makeTransition "x" parent currId
            let trans2 = makeTransition "x" parent (currId + 1)
            let trans3 = makeTransition "x" parent (currId + 2)
            String.concat "\n" [printToDotStr; varCall; pathStr; trans1; trans2; trans3]
        | AST.Print (AST.Var name) ->
            let printCall = makeLabel "x" currId "Print"
            let varCall = makeLabel "x" (currId + 1) name
            let trans1 = makeTransition "x" parent currId
            let trans2 = makeTransition "x" parent (currId + 1)
            String.concat "\n" [printCall; varCall; trans1; trans2]
        | AST.VDecl (AST.Var name, expr) ->
            let decl = makeLabel "x" currId "VDecl"
            let name = makeLabel "x" (currId + 1) name
            let trans1 = makeTransition "x" parent currId
            let trans2 = makeTransition "x" parent (currId + 1)
            
            let currId = currId + 1
            
            let inner =
                match expr with
                | AST.FindAll (str, exp) ->
                    let findAll = makeLabel "x" currId "AST.FindAll"
                    let strLabeled = makeLabel "x" (currId + 1) str
                    let trans1 = makeTransition "x" parent currId
                    let trans2 = makeTransition "x" parent (currId + 1)
                    let expression = processRE (currId, currId + 2) exp
                    let trans3 = makeTransition "x" parent (currId + 2)
                    String.concat "\n" [findAll; strLabeled; trans1; trans2; expression; trans3]
                
                | AST.IsAcceptable (str, exp) ->
                    let isAcceptable = makeLabel "x" currId "AST.IsAcceptable"
                    let strLabeled = makeLabel "x" (currId + 1) str
                    let trans1 = makeTransition "x" parent currId
                    let trans2 = makeTransition "x" parent (currId + 1)
                    let expression = processRE (currId, currId + 2) exp
                    let trans3 = makeTransition "x" parent (currId + 2)
                    String.concat "\n" [isAcceptable; strLabeled; trans1; trans2; expression; trans3]
                
                | AST.RegExp regexp ->
                    processRE (parent, currId) regexp
            
            String.concat "\n" [decl; name; trans1; trans2; inner]
    
    let rec _go id accList statements =
        match statements with
        | [] -> accList
        | stmt :: tail ->
            let treeOfStmt = procStatement (0, id)  stmt
            _go (id + 1) (accList @ [ treeOfStmt ]) tail
    
    let res = _go 1 [] program
    let res' = String.concat "\n" res
    System.IO.File.WriteAllText(outPath, $"digraph G {{\n{ res' }}}")