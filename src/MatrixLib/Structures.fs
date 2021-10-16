namespace MatrixLib

open System.Collections.Generic
open System.Runtime.CompilerServices

module Operators =
    /// Represents operators abstraction
    /// (custom equality, get zero element)
    [<IsReadOnly; Struct>]
    type Operators<'a> =
        /// Generator of zeros defined for type 'a
        val getZero: unit -> 'a
        /// Equality operator defined for type 'a
        val equal: 'a -> 'a -> bool
        new(zero, eq) =
            { getZero = zero
              equal = eq }

        /// Equality to zero defined for type 'a
        member this.EqualToZero elem =
            this.equal (this.getZero()) elem
    
    let toOps getZero equality =
        Operators(getZero, equality)  

    /// Operators defined for common system types
    /// (bool, unit, int, int64, float, decimal)
    module CommonOps =
        let booleanOps = Operators((fun () -> false), (=))
        let uintOps = Operators((fun () -> 0u), (=))
        let intOps = Operators((fun () -> 0), (=))
        let int64Ops = Operators((fun () -> 0L), (=))
        let floatOps = Operators((fun () -> 0.0), (=))
        let decimalOps = Operators((fun () -> 0.0m), (=))
        
        // define basic operations for hash-sets
        let hashSetEq (set1: HashSet<_>) (set2: HashSet<_>) = set1.SetEquals(set2)
        let hashSetOps<'a> = Operators((fun () -> HashSet<'a>()), hashSetEq)

module AlgStructs =
    open Operators
    
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
        member this.EqualToZero elem =
            this.eq (this.zero()) elem
    
    let toSemiring zero equality add mul =
        Semiring(zero, equality, add, mul)
    
    /// Get basic operators from semiring of type 'a
    let getOps (sr: Semiring<_>) =
        Operators(sr.zero, sr.eq)
    
    /// Equal to zero as a lambda
    let equalityToZero =
        // useful if we need to pass equality to zero as parameter 
        fun (sr: Semiring<_>) elem -> sr.zero() |> sr.eq elem
    
    /// Semirings defined for common system types
    module CommonSR =
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
        let private hashSetEq (set1: HashSet<_>) (set2: HashSet<_>) =
            set1.SetEquals(set2)
        
        let private hashSetAdd (set1: HashSet<_>) (set2: HashSet<_>) =
            set1.UnionWith(set2)
            set1
        
        let private hashSetMul (set1: HashSet<_>) (set2: HashSet<_>) =
            set1.IntersectWith(set2)
            set1
        
        let hashSetsSR<'a> =
            let zero = (fun () -> HashSet<'a>())
            Semiring(zero, hashSetEq, hashSetAdd, hashSetMul)
