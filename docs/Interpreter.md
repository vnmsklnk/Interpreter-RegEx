# Interpreter
## Developers

To interpretate your code, at first you need to create an abstract sytax tree by using `Interprter.parseProgramToAST <string>`

Then you can run the `Interpreter.run <AST>` function that returns two dictionaries. The first contains values of all variables in `Interpreter.VType` format, the second has only one key - `"print"` with interpretation result in `string` format.
You can also get a dot file which contains an abstract syntax tree by using `AstToDot.astToDot <output file path> <AST>`

### Another functions

* `processExpression (vDict: Dictionary<AST.VName, VType>) (expression: AST.Expr)` - return a result of a given expression in `VType` format.
* `processStmt (vDict: Dictionary<AST.VName, VType>) (pDict: Dictionary<string, string>) (stmt: AST.Stmt)` - gets an expression from a statement and sets it's value to a dictionaries with variable as a key.

### Example

    ``` f#
    let program = "
    let [a] = (x|a)a
	let [b] = a*

	let [c] = isAcceptable \"a\" [a]&[b]
	let [d] = isAcceptable \"1\" (1*)&(1?)
	let [e] = findAll "byx" a|y

	print [c]
	print [d]
	print [e]"

	let ast = Interprter.parseProgramToAST program
	let _, pDict = Interpreter.run ast
	printfn "%A" pDict.["print"]
    ```

Given code prints
    ```False

    True
    
    [(1, 2)]```
into console.

## Users

There are four console commands in Interpreter-RegEx.

* `--inputfile <file path>` - the path to the file with code.
* `--inputstring <string>` - the string with code.
* `--compute` - returns the result of interpretation of the code
* `--todot <file path>` - saves .dot file with abstract syntax tree to the path specified.
	
Just run "Interpreter-RegEx.exe" from console with given commands.