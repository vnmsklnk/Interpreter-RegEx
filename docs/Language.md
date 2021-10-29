# Language guide

Interpreter-RegEx uses simple programming language to define regular expressions.

## Grammar

Code consists of statements with expressions and variable's names associated with them.
Each arithmetic expression is defined as variable which can be used in other expressions. Value of a variable can be printed in console.

## Statements

There are three statements supported in this language: 

*	`print [<vname>]`
*	`let [<vname>] = <expression>`
*   `printToDot [<vname>: RegExp] <string>`

`print` prints the regular expression corresponding to a given variable, `let` defines a variable and `printToDot` outputs regular expression in .dot format.

* `<vname>` starts with a Latin character, which can be followed by numbers or other letters.
* `<expression>` consists of Latin letters, digits, characters `'-' '.' '/' '+' '~' ';'` and other variables. As operators characters `'(' ')' '*' '|' '?' '&'` can be used.

## Regex type

*    `RSmb of <char>`
*    `RVar of <VName>`
*    `Alt of <Regex * Regex>`
*    `Seq of <Regex * Regex>`
*    `Opt of <Regex>`
*    `Star of <Regex>`
*    `Intersect of <Regex * Regex>`

Existing of `RVar` means that variables can be used in regular expressions.

## Expressions

*    `RegExp of <Regex>`
*    `FindAll of <string * Regex>`
*    `IsAcceptable of <string * Regex>`

`FindAll` searches for all substrings satisfying the specified regular expression, `IsAcceptable` checks whether the string belongs to the specified expression.

## Operators

Available operators in Interpreter-RegEx:

* `*` - Kleene star
* `|` - alternation
* `?` - repeat 0 or 1 times
* `&` - intersect
* `(`, `)` - brackets for grouping


## Code example

	let [a] = (x|a)a
	let [b] = a*

	let [c] = isAcceptable "a" [a]&[b]
	let [d] = isAcceptable "1" (1*)&(1?)
	let [e] = findAll "byx" a|y

	print [c] # output: False
	print [d] # output: True
	print [e] # output: [(1, 2)]
