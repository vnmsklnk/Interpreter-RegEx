namespace MatrixLib.SparseMatrixQT

open MatrixLib.Utils                    // using fasterToNextPowerOfTwo func.
open Quadtrees.QuadtreeTypes            // using Square Region
open Quadtrees.MutableQT                // SparseMatrixQT uses mutable quadtree impl.

/// Square sparse matrix implemented on quadtrees;
/// This implementation uses quadtrees with square regions;
/// The size of this matrix is always a power of two,
/// if specified size is not a power of two
/// -> it will be rounded to next nearest power of two
/// (additional matrix cells are considered zero values).
[<Struct>]
type SparseMatrixQT<'a when 'a: equality> =
    val size: int
    val getZero: unit -> 'a
    val quadtree: Quadtree<'a>
    
    new(tree: Quadtree<'a>, getZero) =
        let sizeX, sizeY = (int tree.Region.SizeX), (int tree.Region.SizeY)
        
        if sizeX <> sizeY then
            invalidArg "tree"
            <| "Only square-region quadtree is allowed;"
                + $" err: sizes differ: {sizeX}x{sizeY}"
        
        let _size = fastToNextPowerOfTwo sizeX
        if _size <> sizeX then
            invalidArg "tree"
            <| "Quadtree sizes should be equal to power of 2"
                + $"instead got: {sizeX}x{sizeY}"
        
        { size = _size
          getZero = getZero
          quadtree = tree }

    new(size, getZero) =
        let _size = fastToNextPowerOfTwo size
        { size = _size
          getZero = getZero
          quadtree =
              { Region = Square(_size)
                Content = Empty } }

    // constructs matrix from existing values
    new(xs: 'a [,], getZero, equalityFunc: 'a -> 'a -> bool) =
        let length1, length2 = Array2D.length1 xs, Array2D.length2 xs

        // rounded size
        let powOfTwoSize =
            max length1 length2
            |> fastToNextPowerOfTwo

        // square region creation
        let resQuadtree =
            powOfTwoSize
            |> Square
            |> emptyTree

        for i in 0 .. length1 - 1 do
            for j in 0 .. length2 - 1 do
                let value = xs.[i, j]
                // insert only non-zero values
                if not (equalityFunc value (getZero())) then
                    resQuadtree
                    |> MutableQT.set (i, j) value 
                    |> ignore

        // matrix constructor
        { size = powOfTwoSize
          quadtree = resQuadtree
          getZero = getZero }

    /// Check if coordinates match matrix boundaries
    member this.InsideMainBounds(i, j) =
        0 <= i && i < this.size && 0 <= j && j < this.size

    /// Access matrix element by its coordinates
    member this.Item
        with get (i, j) =
            if not (this.InsideMainBounds(i, j)) then
                invalidArg "i, j" "index[-es] is out of matrix range"

            match MutableQT.get' (i, j) this.quadtree with
            | None -> this.getZero()
            | Some v -> v

        and set (i, j) value =
            if not (this.InsideMainBounds(i, j)) then
                invalidArg "i, j" "index[-es] is out of matrix range"

            // set the point (x, y)
            // with the specified value inside the quadtree
            // (point and value will be replaced if already exist)
            MutableQT.set (i, j) value this.quadtree |> ignore

[<Struct>]
type Operators<'a> =
    { IsEqual: 'a -> 'a -> bool
      GetZero: unit -> 'a }

module SparseMatrixQT =
    let init size getZero equalityFunc (initializer: int -> int -> 'a) =
        let tree =
            size
            |> fastToNextPowerOfTwo
            |> Square
            |> emptyTree

        for row in 0 .. size - 1 do
            for col in 0 .. size - 1 do
                let value = initializer row col
                // insert only non-zero values
                if not (equalityFunc value (getZero())) then
                    tree
                    |> MutableQT.set (row, col) value
                    |> ignore

        SparseMatrixQT(tree, getZero)

    let iteri iterator (matrix: SparseMatrixQT<_>) =
        MutableQT.iteri' iterator matrix.quadtree

    let iter iterator (matrix: SparseMatrixQT<_>) =
        MutableQT.iter iterator matrix.quadtree

    let map zeroOfOuterType equalityOfOuter mapping (mtx: SparseMatrixQT<_>) =
        let rec _go (qt: Quadtree<_>) =
            match qt.Content with
            | Empty -> emptyTree qt.Region
            | Leaf (point, v) ->
                let mapped = mapping v
                // place only non-zero leaves in mapped tree
                if equalityOfOuter mapped (zeroOfOuterType())
                then emptyTree qt.Region
                else makeTree qt.Region (Leaf(point, mapped))
            | Nodes (nw, ne, sw, se) ->
                let _NW = _go nw
                let _NE = _go ne
                let _SW = _go sw
                let _SE = _go se
                if allSubtreesEmpty (_NW, _NE, _SW, _SE)
                then emptyTree qt.Region
                else makeTree qt.Region (Nodes(_NW, _NE, _SW, _SE))

        let treeQT = _go mtx.quadtree
        SparseMatrixQT(treeQT, zeroOfOuterType)

    let mapi zeroOfOuterType equalityOfOuter mapping (mtx: SparseMatrixQT<_>) =
        let rec _go (qt: Quadtree<_>) =
            match qt.Content with
            | Empty -> emptyTree qt.Region
            | Leaf (point, v) ->
                let x, y = int (fst point), int (snd point)
                let mapped = mapping x y v
                // place only non-zero leaves in mapped tree
                if equalityOfOuter mapped (zeroOfOuterType())
                then emptyTree qt.Region
                else makeTree qt.Region (Leaf(point, mapped))
            | Nodes (nw, ne, sw, se) ->
                let _NW = _go nw
                let _NE = _go ne
                let _SW = _go sw
                let _SE = _go se
                if allSubtreesEmpty (_NW, _NE, _SW, _SE)
                then emptyTree qt.Region
                else makeTree qt.Region (Nodes(_NW, _NE, _SW, _SE))

        let treeQT = _go mtx.quadtree
        SparseMatrixQT(treeQT, zeroOfOuterType)

    let toArray2D zero (mtx: SparseMatrixQT<_>) =
        let values = Array2D.create mtx.size mtx.size zero
        iteri (fun x y elem ->
                if elem <> zero then
                    values.[x, y] <- elem) mtx
        values