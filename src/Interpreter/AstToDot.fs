module Interpreter.AstToDot

let makeLabel id label =
    $"x%d{id} [shape = ellipse, label = \"%s{label}\"];\n"

let makeTransition fromId toId =
    $"\t\"x%d{fromId}\" -> \"x%d{toId}\";\n"
        
let astToDot outPath (program: AST.Program) =
    
    let helper currId parentId =
        let varCall = makeLabel currId "Regex.RVar"
        let toThis = makeTransition parentId currId
        
        ()
        
        
    
    let procStatement (parentId: int, currId: int) (stmt: AST.Stmt) =
        let mutable currId = currId
        
        let rec processRE parentId expr: string =
            match expr with
            | AST.RSmb symbol ->
                let rSmb = makeLabel currId (string symbol)
                let toThis = makeTransition parentId currId
                currId <- currId + 1
                String.concat "\n" [rSmb; toThis] 
            
            | AST.RVar (AST.Var name) ->
                let varCall = makeLabel currId "Regex.RVar"
                let toThis = makeTransition parentId currId
                let parent = currId
                
                currId <- currId + 1
                let varName = makeLabel currId name
                let transL = makeTransition parent currId
                
                String.concat "\n" [toThis; varCall; varName; transL]
            
            | AST.Opt regex ->
                let optLabel = makeLabel currId "Regex.Opt"
                let toThis = makeTransition parentId currId
                let parent = currId
                
                currId <- currId + 1
                let next = processRE parent regex
                
                String.concat "\n" [optLabel; toThis; next]
            
            | AST.Star regex ->
                let starLabel = makeLabel currId "Regex.Star"
                let toThis = makeTransition parentId currId
                let parent = currId
                
                currId <- currId + 1
                let next = processRE parent regex
                String.concat "\n" [starLabel; toThis; next]
            
            | AST.Seq (regex1, regex2) ->
                let seqLabel = makeLabel currId "Regex.Seq"
                let toThis = makeTransition parentId currId
                let parent = currId
                
                currId <- currId + 1
                let first = processRE parent regex1
                
                currId <- currId + 1
                let second = processRE parent regex2
                
                String.concat "\n" [seqLabel; toThis; first; second]
                
            | AST.Alt (regex1, regex2) ->
                let alt = makeLabel currId "Regex.Alt"
                let toThis = makeTransition parentId currId
                let parent = currId
                
                currId <- currId + 1
                let first = processRE parent regex1
                
                currId <- currId + 1
                let second = processRE parent regex2
                
                String.concat "\n" [alt; toThis; first; second]
            
            | AST.Intersect (regex1, regex2) ->
                let intersect = makeLabel currId "Regex.Intersect"
                let toThis = makeTransition parentId currId
                let parent = currId
                
                currId <- currId + 1
                let first = processRE parent regex1
                
                currId <- currId + 1
                let second = processRE parent regex2
                String.concat "\n" [intersect; toThis; first; second]
        
        match stmt with
        | AST.PrintToDot (AST.Var name, pathString) ->
            let printToDotStr = makeLabel currId "PrintToDot"
            let parent = currId
            
            currId <- currId + 1
            let varName = makeLabel currId name
            
            currId <- currId + 1
            let pathStr = makeLabel currId pathString
            
            let toThis = makeTransition parentId parent
            let toVarName = makeTransition parent (parent + 1)
            let toPath = makeTransition parent (parent + 2)
            String.concat "\n" [printToDotStr; varName; pathStr; toThis; toVarName; toPath], currId
        
        | AST.Print (AST.Var name) ->
            let printCall = makeLabel currId "Print"
            let parent = currId
            
            currId <- currId + 1
            let varCall = makeLabel currId name
            let toThis = makeTransition parentId parent
            let toName = makeTransition parent currId
            String.concat "\n" [printCall; varCall; toThis; toName], currId
        
        | AST.VDecl (AST.Var name, expr) ->
            let decl = makeLabel currId "VDecl"
            let parent = currId
            
            currId <- currId + 1
            let nameLabel = makeLabel currId name
            
            let toThis = makeTransition parentId parent
            let toName = makeTransition parent currId
            
            currId <- currId + 1
            
            let inner =
                match expr with
                | AST.FindAll (str, exp) ->
                    let findAll = makeLabel currId "AST.FindAll"
                    let _parent = currId
                    
                    currId <- currId + 1
                    let strLabeled = makeLabel currId str
                    
                    let trans1 = makeTransition parent _parent
                    let trans2 = makeTransition _parent (_parent + 1)
                    currId <- currId + 1
                    let exprId = currId
                    let expression = processRE _parent exp
                    let trans3 = makeTransition _parent exprId
                    String.concat "\n" [findAll; strLabeled; trans1; trans2; expression; trans3]
                
                | AST.IsAcceptable (str, exp) ->
                    let isAcceptable = makeLabel currId "AST.IsAcceptable"
                    let _parent = currId
                    
                    currId <- currId + 1
                    let strLabeled = makeLabel currId str
                    
                    let trans1 = makeTransition parent _parent
                    let trans2 = makeTransition _parent (_parent + 1)
                    currId <- currId + 1
                    let expression = processRE _parent exp
                    String.concat "\n" [isAcceptable; strLabeled; trans1; trans2; expression]
                
                | AST.RegExp regexp ->
                    processRE parent regexp
            
            String.concat "\n" [decl; nameLabel; toThis; toName; inner], currId
    
    let rec _go id accList statements =
        match statements with
        | [] -> accList
        | stmt :: tail ->
            let treeOfStmt, id = procStatement (0, id)  stmt
            _go (id + 1) (accList @ [ treeOfStmt ]) tail
    
    let res = _go 1 [] program
    let res' = String.concat "\n" res
    System.IO.File.WriteAllText(outPath, $"digraph G {{\n{ res' }}}")