module Interpreter.AstToDot

/// Returns dot string with labeled element
let makeLabel id label =
    $"\tx%d{id} [shape = ellipse, label = \"%s{label}\"];"

/// Returns transition from A to B defined in dot
let makeTransition fromId toId =
    $"\t\"x%d{fromId}\" -> \"x%d{toId}\";"

/// Writes AST converted to dot language (graphviz) digraph at specified path
let astToDot outPath (program: AST.Program) =
    let procStatement (parentId: int, currId: int) (stmt: AST.Stmt) =
        let mutable currId = currId        
        /// Converts RegEx to dot string recursively
        let rec processRE parentId expr: string =
            /// Builds dot transitions for binary regex operators
            let oneRegexOperation str regex =
                let label = makeLabel currId str
                let toLabeled = makeTransition parentId currId
                let parent = currId
                currId <- currId + 1
                let next = processRE parent regex
                String.concat "\n" [toLabeled; label; next]
            
            /// Builds dot transitions for binary regex operators
            let twoRegexesOperation str regex1 regex2 =
                let label = makeLabel currId str
                let toLabeled = makeTransition parentId currId
                let parent = currId
                currId <- currId + 1
                let firstRE = processRE parent regex1
                currId <- currId + 1
                let secondRE = processRE parent regex2
                String.concat "\n" [toLabeled; label; firstRE; secondRE]
                
            match expr with
            | AST.RSmb symbol ->
                let reSmb = makeLabel currId (string symbol)
                let toSmb = makeTransition parentId currId
                currId <- currId + 1
                String.concat "\n" [toSmb; reSmb] 
            
            | AST.RVar (AST.Var name) ->
                let reVar = makeLabel currId "Regex.RVar"
                let toReVar = makeTransition parentId currId
                let parent = currId
                currId <- currId + 1
                let varName = makeLabel currId name
                let toName = makeTransition parent currId
                String.concat "\n" [toReVar; reVar; varName; toName]
            
            | AST.Opt regex -> oneRegexOperation "Regex.Opt" regex
            | AST.Star regex -> oneRegexOperation "Regex.Star" regex
            | AST.Seq (regex1, regex2) -> twoRegexesOperation "Regex.Seq" regex1 regex2
            | AST.Alt (regex1, regex2) -> twoRegexesOperation "Regex.Alt" regex1 regex2
            | AST.Intersect (regex1, regex2) -> twoRegexesOperation "Regex.Intersect" regex1 regex2
        
        match stmt with
        | AST.PrintToDot (AST.Var name, pathString) ->
            let printCall = makeLabel currId "PrintToDot"
            let parent = currId
            
            currId <- currId + 1
            let varName = makeLabel currId name
            currId <- currId + 1
            let pathStr = makeLabel currId pathString
            
            let toPrintCall = makeTransition parentId parent
            let toVarName = makeTransition parent (parent + 1)
            let toPath = makeTransition parent (parent + 2)
            String.concat "\n" [toPrintCall; printCall; varName; pathStr; toVarName; toPath], currId
        
        | AST.Print (AST.Var name) ->
            let printCall = makeLabel currId "Print"
            let parent = currId
            
            currId <- currId + 1
            let varCall = makeLabel currId name
            let toPrintCall = makeTransition parentId parent
            let toName = makeTransition parent currId
            String.concat "\n" [toPrintCall; printCall; varCall; toName], currId
        
        | AST.VDecl (AST.Var name, expr) ->
            let varDecl = makeLabel currId "VDecl"
            let parent = currId
            
            currId <- currId + 1
            let nameLabel = makeLabel currId name
            let toThis = makeTransition parentId parent
            let toName = makeTransition parent currId
            
            currId <- currId + 1
            
            let inner =
                match expr with
                | AST.FindAll (str, regex) ->
                    let findAll = makeLabel currId "AST.FindAll"
                    let _parent = currId
                    
                    currId <- currId + 1
                    let strLabeled = makeLabel currId str
                    
                    let trans1 = makeTransition parent _parent
                    let trans2 = makeTransition _parent (_parent + 1)
                    currId <- currId + 1
                    
                    let reId = currId
                    let expression = processRE _parent regex
                    let trans3 = makeTransition _parent reId
                    String.concat "\n" [findAll; strLabeled; trans1; trans2; expression; trans3]
                
                | AST.IsAcceptable (str, regex) ->
                    let isAcceptable = makeLabel currId "AST.IsAcceptable"
                    let _parent = currId
                    
                    currId <- currId + 1
                    let strLabeled = makeLabel currId str
                    
                    let trans1 = makeTransition parent _parent
                    let trans2 = makeTransition _parent (_parent + 1)
                    currId <- currId + 1
                    
                    let expression = processRE _parent regex
                    String.concat "\n" [isAcceptable; strLabeled; trans1; trans2; expression]
                
                | AST.RegExp regexp ->
                    processRE parent regexp
            
            String.concat "\n" [varDecl; nameLabel; toThis; toName; inner], currId
    
    let rec _go id accList statements =
        match statements with
        | [] -> accList
        | stmt :: tail ->
            let treeOfStmt, id = procStatement (0, id)  stmt
            _go (id + 1) (accList @ [ treeOfStmt ]) tail
    
    let digraphBodyStr = _go 1 [] program
    System.IO.File.WriteAllText(outPath, $"digraph G {{\n{ (String.concat "\n" digraphBodyStr) }\n}}")