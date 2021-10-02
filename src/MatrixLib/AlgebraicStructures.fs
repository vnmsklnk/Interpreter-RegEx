module MatrixLib.AlgebraicStructures

[<AutoOpen>]
module Simplified =
    /// Monoid type with associative binary operators and an identity element.
    type Monoid<'a> =
        { GetIdentityElement: unit -> 'a
          AssociativeBinOp: 'a -> 'a -> 'a }

    /// Semiring type with generic zero value (of specified type)
    /// and two binary operators: addition, multiplication
    (* Note: this is not an actual algebraic semiring, because it does not fully implement monoids:
        - type does not declare explicit additive identity;
        - type does not declare explicit multiplicative identity;
        In some cases, GenericZero is assumed to be additive identity. *)
    type Semiring<'a> =
        { GetGenericZero: unit -> 'a
          Addition: 'a -> 'a -> 'a
          Multiplication: 'a -> 'a -> 'a }