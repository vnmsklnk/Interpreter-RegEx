---
hide:
  - navigation
---
# Github repository structure

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