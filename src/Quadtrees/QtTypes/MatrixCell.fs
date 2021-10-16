namespace Quadtrees.QtTypes

module MatrixCell =
    open Quadtrees.Utils
    open Direction
    open Region

    // IsReadOnly attribute for structs
    open System.Runtime.CompilerServices

    /// Stores the parameters
    /// (center(X,Y), size)
    /// of a matrix cell
    [<IsReadOnly; Struct>]
    type CellParams =
        val X: float
        val Y: float
        val Size: int
        new (center, size) =
            { X = fst center
              Y = snd center
              Size = size }
        
        member this.Center = this.X, this.Y

    [<IsReadOnly; Struct>]
    type MatrixCell =
        val size: int
        val center: float * float
        
        new (prm: CellParams) =
            checkSize prm.Size
            { center = prm.Center
              size = prm.Size }
        
        new(x, y, size) =
            checkSize size
            { center = (x, y); size = size }

        new(size: int) =
            checkSize size
            let n = float size
            { center = n / 2.0, n / 2.0
              size = size }

        override this.ToString() =
            $"Cell [{this.size} x {this.size}], center at: {this.center}"
        
        interface IRegion<int> with
            override this.SizeX = this.size
            override this.SizeY = this.size

            /// Checks that the point is inside the current square
            override this.Contains(X, Y) =
                let x, y, _half = float X, float Y, this.size |> half
                match abs (x - fst this.center) <= _half, abs (y - snd this.center) <= _half with
                | true, true -> true
                | _, _ -> false

            /// Returns direction for further region search
            override this.GetDirection(X, Y) =
                match float X, float Y with
                | x, y when x >= fst this.center && y >= snd this.center -> SE
                | x, y when x >= fst this.center && y < snd this.center -> SW
                | x, y when x < fst this.center && y >= snd this.center -> NE
                | _, _ -> NW

            /// Checks if minimum region size is reached;
            /// In this impl. it is assumed that a square of size = 1.0
            /// cannot be split to subregions.
            override this.SplitPossible() = half this.size >= 1.0

            /// Returns new subregion placed at specified direction
            override this.Split(direction: Direction) =
                // new region size (size of new subregion)
                // is equal to half size of current region
                let halfSize = half this.size
                if halfSize < 1.0 then
                    failwith "Could not be split; minimum size reached"

                let quarter = halfSize / 2.0
                let cntX, cntY = this.center
                match direction with
                | NW -> MatrixCell(cntX - quarter, cntY - quarter, this.size / 2) :> _
                | NE -> MatrixCell(cntX - quarter, cntY + quarter, this.size / 2) :> _
                | SW -> MatrixCell(cntX + quarter, cntY - quarter, this.size / 2) :> _
                | SE -> MatrixCell(cntX + quarter, cntY + quarter, this.size / 2) :> _

            /// Get member-wise copy of square region
            override this.Copy() =
                this.MemberwiseClone() :?> IRegion<_>

    /// Calculates subregion parameters for specified cell
    let getSubsParams (cell: CellParams) =
        let half, quarter = cell.Size / 2, float cell.Size / 4.0
        let nw, ne, sw, se =
            CellParams((cell.X - quarter, cell.Y - quarter), half),
            CellParams((cell.X - quarter, cell.Y + quarter), half),
            CellParams((cell.X + quarter, cell.Y - quarter), half),
            CellParams((cell.X + quarter, cell.Y + quarter), half)
        
        // return calculated parameters
        // for each subregion
        (nw, ne, sw, se)

    /// Downcast regions to matrix cells if possible;
    /// otherwise fail with exception
    let regionsAreCells (reg1: IRegion<_>) (reg2: IRegion<_>) =
        match (reg1 :? MatrixCell) && (reg2 :? MatrixCell) with
        | true -> reg1 :?> MatrixCell, reg2 :?> MatrixCell
        | false ->
            "Error: regions expected to have type 'MatrixCell';"
                + $" instead got: {reg1.GetType()}, {reg2.GetType()}" |> failwith
            
    /// Returns correct cell parameters if specified matrix cells can be multiplied;
    /// otherwise fails with exception
    let cellsCanBeMultiplied (cell1: MatrixCell) (cell2: MatrixCell) =
        // check sizes (multiplication cond.)
        if cell1.size <> cell2.size then
            failwith "Error: cells can not be multiplied -- different sizes"
        // check regions centers
        if cell1.center <> cell2.center then
            failwith "Error: cells can not be multiplied -- different region centers"
        
        CellParams(cell1.center, cell1.size)
