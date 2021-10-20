module AST

type VName = Var of string

/// Supported regex syntax
type Regex =
    | RSmb of char
    | RVar of VName
    | Alt of Regex * Regex
    | Seq of Regex * Regex
    | Opt of Regex
    | Star of Regex
    | Intersect of Regex * Regex

/// Operations on regex
type Expr =
    | RegExp of Regex
    | FindAll of string * Regex
    | IsAcceptable of string * Regex

/// General language syntax 
type Stmt =
    | Print of VName
    | PrintToDot of VName * string
    | VDecl of VName * Expr

/// Program consists of statements
type Program = List<Stmt>
