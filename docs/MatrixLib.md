# Matrix Library

BigInt is a long arithmetic library used in the Arithm Interpeter.

## Type

An instanse of BigInt consists of `Sign of <Sign>` and `Digits of <MyList<int>>`.
`Sign` type has only two values - `Positive` and `Negative`.
`MyList<int>` consists of integers from 0 to 9

## Functions

* `equal (x:BigInt) (y:BigInt)` - returns result of "x = y" expression
* `getSign (x:BigInt)` - returns 1 for positive and -1 for negative numbeк
* `reverseSign (x:BigInt)` - changes a sign of a number
* `equalize (x:MyList<int>, y:MyList<int>)` - adds zeroes to a begining of one list until their lengths are equal
* `delZeroHead (l:MyList<int>)` - deletes all zeroes from the begining of a list
* `addZeroes (c:int) (l:MyList<int>)` - adds `c` zeroes to a begining
* `notLesser (x:MyList<t>) (y:MyList<t>)` - returns true if x is not lesser than y lexicographic order
* `manageRemainders (_arg1:MyList<int>)` - moves remainders to the next rank(for the digits in decimal system)
* `sumOrSub (x:BigInt) (y:BigInt) (operator:int -> int -> int)` - applies a given operator(usually "+" or "-") to a numbers
* `sum (x:BigInt) (y:BigInt)` - returns sum of two numbers
* `sub (x:BigInt) (y:BigInt)` - returns a result of subtraction of two numbers
* `mul (x:BigInt) (y:BigInt)` - returns a result of multiplication of two numbers
* `divOrRem (x:BigInt) (y:BigInt)` - returns a pair of remainder and whole part of division in `BigInt` type
* `div (x:BigInt) (y:BigInt)` - returns a whole part of division
* `rem (x:BigInt) (y:BigInt)` - returns a remainder from division
* `power (x:BigInt) (y:BigInt)` - returns a result of exponentiation 
* `toBinary (x:BigInt)` - converts a number to it's binary representation 
* `stringToBigInt (n:string)` - converts a `string` to `BigInt`
* `bigIntToString (n:BigInt)` - converts a `BigInt` to `string` 
* `abs (x:BigInt)` - return an absolute value of a number  