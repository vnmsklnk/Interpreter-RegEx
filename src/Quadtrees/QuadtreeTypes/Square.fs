namespace Quadtrees.QuadtreeTypes

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type Square =
    val private size: float
    val private center: float * float
    new(x, y, size) = { center = (x, y); size = size }

    new(size) =
        { center = size / 2.0, size / 2.0
          size = size }

    new(size: int) =
        // convert to float
        let sizeFloat = float size

        { center = sizeFloat / 2.0, sizeFloat / 2.0
          size = sizeFloat }

    member this.HalfSize = this.size / 2.0

    override this.ToString() =
        $"Square [{this.size} x {this.size}], center at: {this.center}"

    interface Region with
        member this.Center = this.center
        member this.SizeX = this.size
        member this.SizeY = this.size

        /// Checks that the point is inside the current square
        member this.Contains(X, Y) =
            match abs (X - fst this.center) <= this.HalfSize, abs (Y - snd this.center) <= this.HalfSize with
            | true, true -> true
            | _, _ -> false

        /// Returns direction for further region search
        member this.GetDirection(X, Y) =
            match X, Y with
            | x, y when x >= fst this.center && y >= snd this.center -> SE
            | x, y when x >= fst this.center && y < snd this.center -> SW
            | x, y when x < fst this.center && y >= snd this.center -> NE
            | _, _ -> NW

        /// Checks if minimum region size is reached;
        /// In this impl. it is assumed that a square of size = 1.0
        /// cannot be split to subregions.
        member this.SplitPossible() = this.HalfSize >= 1.0

        /// Returns new subregion placed at specified direction
        member this.Split(direction: Direction) =
            // new region size (size of new subregion)
            // is equal to half size of current region
            if this.HalfSize < 1.0 then
                failwith "Could not split minimum size reached"

            let quarter = this.HalfSize / 2.0

            match direction with
            | NW -> Square(fst this.center - quarter, snd this.center - quarter, this.HalfSize) :> _
            | NE -> Square(fst this.center - quarter, snd this.center + quarter, this.HalfSize) :> _
            | SW -> Square(fst this.center + quarter, snd this.center - quarter, this.HalfSize) :> _
            | SE -> Square(fst this.center + quarter, snd this.center + quarter, this.HalfSize) :> _

        /// Get member-wise copy of square region
        member this.Copy() =
            this.MemberwiseClone() :?> Region
        
        /// Get square region data in in single struct        
        member this.Parameters() =
            { Cnt = fst this.center, snd this.center
              SizeX = this.size
              SizeY = this.size }
