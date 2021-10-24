namespace MatrixLib

open System.Runtime.CompilerServices

/// Represents operators abstraction
/// (custom equality, get zero element)
[<IsReadOnly; Struct>]
type Operators<'a> =
    /// Generator of zeros defined for type 'a
    val getZero: unit -> 'a
    /// Equality operator defined for type 'a
    val equal: 'a -> 'a -> bool
    new(zero, eq) = { getZero = zero; equal = eq }

    /// Equality to zero defined for type 'a
    member this.EqualToZero elem = this.equal (this.getZero ()) elem

/// Monoid with
/// associative binary operator
/// and an identity element
[<IsReadOnly; Struct>]
type Monoid<'a> =
    { GetIdentityElement: unit -> 'a
      AssociativeBinOp: 'a -> 'a -> 'a }

(* Note: this is not an actual impl. of algebraic semiring,
   because it does not fully implement monoids:
    - type does not declare explicit additive identity;
    - type does not declare explicit multiplicative identity;
*)

/// Semiring type with generic zero value (of specified type)
/// and two binary operators: addition, multiplication
[<IsReadOnly; Struct>]
type Semiring<'a> =
    /// zero - represents (additive) identity elem. in semiring of type 'a
    /// (zero element of type 'a)
    val zero: unit -> 'a
    /// eq - represents equality ('=' operator) of two elements in semiring of type 'a
    val eq: 'a -> 'a -> bool
    /// add - represents addition ('+' operator) in semiring of type 'a
    val add: 'a -> 'a -> 'a
    /// mul - represents multiplication ('*' operator) in semiring of type 'a
    val mul: 'a -> 'a -> 'a

    new(getZero, isEqual, addOp, mulOp) =
        { zero = getZero
          eq = isEqual
          add = addOp
          mul = mulOp }

    /// Equality to zero in semiring of type 'a
    member this.EqualToZero elem = this.eq (this.zero ()) elem

/// Basic functions for operators and semiring
module Structures =
    /// Creates operators instance (just a shortcut/alias for `Operators` constructor)
    let toOps getZero equality = Operators(getZero, equality)

    /// Creates semiring from specified operations
    let toSemiring zero equality add mul = Semiring(zero, equality, add, mul)

    /// Get basic operators from semiring of type 'a
    let getOps (sr: Semiring<_>) = Operators(sr.zero, sr.eq)

    /// Equal to zero as a lambda
    let equalityToZero =
        // useful if we need to pass equality to zero as parameter
        fun (sr: Semiring<_>) elem -> sr.zero () |> sr.eq elem

/// Operators defined for common system types
/// (bool, unit, int, int64, float, decimal)
module Operators =
    open System.Collections.Generic
    let booleanOps = Operators((fun () -> false), (=))
    let uintOps = Operators((fun () -> 0u), (=))
    let intOps = Operators((fun () -> 0), (=))
    let int64Ops = Operators((fun () -> 0L), (=))
    let floatOps = Operators((fun () -> 0.0), (=))
    let decimalOps = Operators((fun () -> 0.0m), (=))

    /// Defines equality operator for hash sets
    let hashSetEq (setA: HashSet<_>) (setB: HashSet<_>) = setA.SetEquals(setB)
    
    /// Hash set operators;
    /// - getZero returns new instance of HashSet<_>
    /// - SetEquals method used to define equality
    let hashSetOps<'a> =
        Operators((fun () -> HashSet<'a>()), hashSetEq)

/// Semirings defined for common system types
module Semirings =
    open System.Collections.Generic
    let booleanSR = Semiring((fun () -> false), (=), (||), (&&))
    let uintSR = Semiring((fun () -> 0u), (=), (+), (*))
    let intSR = Semiring((fun () -> 0), (=), (+), (*))
    let int64SR = Semiring((fun () -> 0L), (=), (+), (*))
    let floatSR = Semiring((fun () -> 0.0), (=), (+), (*))
    let decimalSR = Semiring((fun () -> 0.0m), (=), (+), (*))

    // To define semiring for
    // HashSet (from System.Collections.Generic) of type 'a
    // we first need to define operations for sets:
    // -> equality of sets (=)
    // -> addition of sets (union)
    // -> multiplication of sets (intersection)
    /// Addition operator defined for hash sets
    let hashSetAdd (setA: HashSet<_>) (setB: HashSet<_>) =
        // hash set is mutable collection
        // so we need to create a copy of existing instance
        let updated = HashSet(setA)
        updated.UnionWith setB
        updated

    /// Multiplication operator defined for hash sets
    let hashSetMul (setA: HashSet<_>) (setB: HashSet<_>) =
        // hash set is mutable collection
        // so we need to create a copy of existing instance
        let updated = HashSet(setA)
        updated.IntersectWith setB
        updated

    let hashSetsSR<'a> =
        let zero = (fun _ -> HashSet<'a>())
        Semiring(zero, Operators.hashSetEq, hashSetAdd, hashSetMul)
