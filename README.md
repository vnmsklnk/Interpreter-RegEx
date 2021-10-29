# Interpreter-RegEx

||Badge|
|------|:------:|
|**Build Status**|[![GitHub Actions](https://github.com/IvanMoskalenko/Interpreter-RegEx/workflows/Build/badge.svg?branch=master)](https://github.com/IvanMoskalenko/Interpreter-RegEx/actions?query=branch%3Amaster) |
|**Build History**|[![Build History](https://buildstats.info/github/chart/IvanMoskalenko/Interpreter-RegEx)](https://github.com/IvanMoskalenko/Interpreter-RegEx/actions?query=branch%3Amaster) |
|**Target Framework**|[![Targets](https://img.shields.io/badge/.NET%20-5-green.svg)](https://docs.microsoft.com/ru-ru/dotnet/core/introduction)|
|**Contacts**|[![Telegram](https://raw.githubusercontent.com/Patrolavia/telegram-badge/master/ask.svg)](https://t.me/vnmsklnk)|


Interpreter-RegEx is a tool for elementary programming language created for processing regular expressions. There are Quadtree and Matrix operations libraries also. Interpreter has user-friendly console interface; Matrix operations and Quadtree libraries can be used only by developers.

## Getting Started

You can install the package with dotnet by following this steps:

* Add a source and PAT in your nuget.config file:

  dotnet nuget add source --username <YOUR_USERNAME> --password <PAT> --store-password-in-clear-text --name github "https://nuget.pkg.github.com/IvanMoskalenko/index.json"

* Install the package by using this command:

  dotnet add PROJECT package Interpreter-RegEx --version <version>

## Usage and Examples

Each regular expression is defined as variable which can be used in other expressions. Typical program consists of statements with expressions and variables' names associated with them.

Three statements are supported:

	let [var] = expr # Variable declaration; var = { 'a'..'z' | 'A'..'Z' | '0' - '9' }
	print [var] # Statement for outputting variables
	printToDot [var: RegExp] <string> # Statement for outputting regular expressions to .dot file

Example:

	let [a] = (x|a)a
	let [b] = a*

	let [c] = isAcceptable "a" [a]&[b]
	let [d] = isAcceptable "1" (1*)&(1?)
	let [e] = findAll "byx" a|y

	print [c] # output: False
	print [d] # output: True
	print [e] # output: [(1, 2)]

## Documents

Visit [docs](https://ivanmoskalenko.github.io/Interpreter-RegEx/) for full overview of tool.

## Repository structure

```
Interpreter-RegEx
├── .config - dotnet tools
├── .github - GitHub Actions setup 
├── docs - documentation files in .md format
├── src - code of the tool
│	├── Interpreter - interpreter of regular expressions
|	├── Interpreter.Cli - command interface for Interpreter
|	├── MatrixLib - library of matrix operations based on quadtrees
|	└── Quadtrees - quadtrees library
├── tests - tests
|	├── Interpreter.UnitTests - tests for Interpreter
|	└── Quadtrees.UnitTests - tests for MatrixLib and Quadtrees
├── fsharplint.json - linter config
├── mkdocs.yml - MkDocs config
└── Interpreter.sln - solution file
```
	



