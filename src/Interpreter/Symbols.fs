module Interpreter.Symbols

type NFASmb<'t> =
    | Eps
    | Smb of 't

/// Generic regex grammar
type RegEx<'t> =
    | REps
    | RSmb of 't
    | Seq of RegEx<'t> * RegEx<'t>
    | Alt of RegEx<'t> * RegEx<'t>
    | Star of RegEx<'t>
    | Intersect of RegEx<'t> * RegEx<'t> // todo: implement