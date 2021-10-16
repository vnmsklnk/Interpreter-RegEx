namespace Quadtrees.QtTypes

module SquareFloat =
    open Direction
    open Region

    // IsReadOnly attribute for structs
    open System.Runtime.CompilerServices
    
    [<IsReadOnly; Struct>]
    type SquareFloat =
        val size: float
        val center: float * float
        new(x, y, size) = { center = (x, y); size = size }

        new(size) =
            { center = size / 2.0, size / 2.0
              size = size }

        member this.HalfSize = this.size / 2.0

        override this.ToString() =
            $"Square [{this.size} x {this.size}], center at: {this.center}"
        
        interface IRegion<float> with
            override this.SizeX = this.size
            override this.SizeY = this.size

            /// Checks that the point is inside the current square
            override this.Contains(X, Y) =
                match abs (X - fst this.center) <= this.HalfSize, abs (Y - snd this.center) <= this.HalfSize with
                | true, true -> true
                | _, _ -> false

            /// Returns direction for further region search
            override this.GetDirection(X, Y) =
                match X, Y with
                | x, y when x >= fst this.center && y >= snd this.center -> SE
                | x, y when x >= fst this.center && y < snd this.center -> SW
                | x, y when x < fst this.center && y >= snd this.center -> NE
                | _, _ -> NW

            /// Checks if minimum region size is reached;
            /// In this impl. it is assumed that a square of size = 1.0
            /// cannot be split to subregions.
            override this.SplitPossible() = this.HalfSize >= 1.0

            /// Returns new subregion placed at specified direction
            override this.Split(direction: Direction) =
                // new region size (size of new subregion)
                // is equal to half size of current region
                if this.HalfSize < 1.0 then
                    failwith "Could not split minimum size reached"

                let quarter = this.HalfSize / 2.0

                match direction with
                | NW -> SquareFloat(fst this.center - quarter, snd this.center - quarter, this.HalfSize) :> _
                | NE -> SquareFloat(fst this.center - quarter, snd this.center + quarter, this.HalfSize) :> _
                | SW -> SquareFloat(fst this.center + quarter, snd this.center - quarter, this.HalfSize) :> _
                | SE -> SquareFloat(fst this.center + quarter, snd this.center + quarter, this.HalfSize) :> _

            /// Get member-wise copy of square region
            override this.Copy() =
                this.MemberwiseClone() :?> IRegion<_>
