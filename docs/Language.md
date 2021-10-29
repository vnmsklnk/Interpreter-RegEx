# Language guide

Arithm uses simple programming language to define complex arithmetic expressions.

## Grammar

Code consists of statements with expressions and variable's names associated with them.
Each arithmetic expression is defined as variable which can be used in other expressions. Value of a variable can be printed in console

## Statements

There are only two statements supported in this language: 

*	`print <vname>`
*	`let <vname> = <expression>`

`print` prints a result of arithmetic expression corresponding to a given variable and `let` defines a variable

* `<vname>` starts with a Latin character, which can be followed by numbers or other letters
* `<expression>` consists of numbers, other variables and arithmetic operators such as `+, -, *, /, %, ^, ~, (, ), |`

## Expressions 

*	`Num of <BigInt>`
*	`NVar of <VName>`
*	`Sum of <Expression * Expression>`
*	`Sub of <Expression * Expression>`
*	`Mul of <Expression * Expression>`
*	`Div of <Expression * Expression>`
*	`Rem of <Expression * Expression>`
*	`Pow of <Expression * Expression>`
*	`Bin of <Expression>`
*	`Abs of <Expression>`

Existing of `NVar` expression means that a variable can be used in expressions. Another expressions are self-exlanatory

## Operators

This is a list of available operators and corresponding expressions:

* `+` - sum; `Sum`
* `-` - subtract; also acts as unary minus if immediatly followed by number `Sub`
* `*` - multiply `Mul`
* `/` - integer division `Div`
* `%` - remainder division `Rem`
* `^` - power `Pow`
* `~` - converts a number to its binary representation `Bin`
* `(`, `)` - brackets to control operation priority
* `|` - acts as brackets while returning an absolute value of expression `Abs`

## Code example

	let x = |12 - 7 * 8| / -3
	let y = 8 - x
	print y

* All code can be written in a single string