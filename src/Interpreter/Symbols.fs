module Interpreter.Symbols

/// Defines NFA symbol,
/// NFA symbol could be epsilon, character or something of type 'a
type NFASmb<'t> =
    | Eps
    | Smb of 't

/// Regex grammar
type RegEx<'t> =
    | REps
    | RSmb of 't
    | Seq of RegEx<'t> * RegEx<'t>
    | Alt of RegEx<'t> * RegEx<'t>
    | Star of RegEx<'t>
    | Intersect of RegEx<'t> * RegEx<'t>