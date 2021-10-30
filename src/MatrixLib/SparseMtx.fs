namespace MatrixLib

open Quadtrees.Utils
open Quadtrees.QtTypes.MatrixCell
open Quadtrees.MutableQT

/// Square sparse matrix implemented on quadtrees;
/// This implementation uses quadtrees
/// with square regions (matrix cells);
/// The size of this matrix is always a power of two,
/// if specified size is not a power of two
/// -> it will be rounded to next nearest power of two
/// (additional matrix cells are considered zero values).
[<Struct>]
type SparseMtx<'a when 'a: equality> =
    val size: int
    val ops: Operators<'a>
    val tree: Quadtree<int, 'a>
    
    new(tree: Quadtree<_,_>, operators) =
        let reg = tree.Region :?> MatrixCell
        // if region is a matrix cell
        // -> create sparse matrix
        // from specified quadtree
        { size = reg.size
          ops = operators
          tree = tree }

    new(size, operators) =
        let _size = fastToNextPowerOfTwo size
        { size = _size
          ops = operators
          tree =
              { Region = MatrixCell(_size)
                Content = Empty } }

    // constructs matrix from existing values
    new(xs: 'a [,], operators: Operators<'a>) =
        let length1, length2 = Array2D.length1 xs, Array2D.length2 xs

        // rounded size
        let powOfTwoSize =
            max length1 length2
            |> fastToNextPowerOfTwo

        // square region creation
        let resQuadtree =
            powOfTwoSize
            |> MatrixCell
            |> emptyTree

        for i in 0 .. length1 - 1 do
            for j in 0 .. length2 - 1 do
                let value = xs.[i, j]
                // insert only non-zero values
                if not (operators.EqualToZero value) then
                    resQuadtree
                    |> MutableQT.insert (i, j) value 
                    |> ignore

        // matrix constructor
        { size = powOfTwoSize
          ops = operators
          tree = resQuadtree }

    /// Check if coordinates match matrix boundaries
    member this.InsideMainBounds(i, j) =
        0 <= i && i < this.size && 0 <= j && j < this.size

    /// Access matrix element by its coordinates
    member this.Item
        with get (i, j) =
            if not (this.InsideMainBounds(i, j)) then
                invalidArg "i, j" "index[-es] is out of matrix range"
            
            match MutableQT.get (i, j) this.tree with
            | None -> this.ops.getZero()
            | Some v -> v

        and set (i, j) value =
            if not (this.InsideMainBounds(i, j)) then
                invalidArg "i, j" "index[-es] is out of matrix range"

            // set the point (x, y)
            // with the specified value inside the quadtree
            // (point and value will be replaced if already exist)
            this.tree
            |> MutableQT.insert (i, j) value
            |> ignore

/// SparseMtx standard functions
module SparseMtx =
    let init size (ops: Operators<'a>) (initializer: int -> int -> 'a) =
        let tree =
            size
            |> fastToNextPowerOfTwo
            |> MatrixCell
            |> emptyTree

        for row in 0 .. size - 1 do
            for col in 0 .. size - 1 do
                let value = initializer row col
                // insert only non-zero values
                if not (ops.EqualToZero value) then
                    tree
                    |> MutableQT.insert (row, col) value
                    |> ignore

        SparseMtx(tree, ops)

    let iteri iterator (matrix: SparseMtx<_>) =
        MutableQT.iteri iterator matrix.tree

    let iter iterator (matrix: SparseMtx<_>) =
        MutableQT.iter iterator matrix.tree

    let map (outerTypeOps: Operators<'b>) mapping (mtx: SparseMtx<'a>) =
        let rec loop (qt: Quadtree<_,_>) =
            match qt.Content with
            | Empty -> emptyTree qt.Region
            | Leaf (point, value) ->
                let mapped = mapping value
                // place only non-zero leaves in mapped tree
                if outerTypeOps.EqualToZero mapped
                then emptyTree qt.Region
                else makeTree qt.Region (Leaf(point, mapped))
            | Nodes (nw, ne, sw, se) ->
                let _NW, _NE, _SW, _SE =
                    // map all subregions
                    loop nw, loop ne, loop sw, loop se 
                if allSubtreesEmpty (_NW, _NE, _SW, _SE)
                then emptyTree qt.Region
                else makeTree qt.Region (Nodes(_NW, _NE, _SW, _SE))

        let treeQT = loop mtx.tree
        SparseMtx(treeQT, outerTypeOps)

    let mapi (outerTypeOps: Operators<'b>) mapping (mtx: SparseMtx<'a>) =
        let rec loop (qt: Quadtree<_,_>) =
            match qt.Content with
            | Empty -> emptyTree qt.Region
            | Leaf ((x, y), value) ->
                let mapped = mapping x y value
                // place only non-zero leaves in mapped tree
                if outerTypeOps.EqualToZero mapped
                then emptyTree qt.Region
                else makeTree qt.Region (Leaf((x, y), mapped))
            | Nodes (nw, ne, sw, se) ->
                let _NW, _NE, _SW, _SE =
                    // map all subregions
                    loop nw, loop ne, loop sw, loop se
                if allSubtreesEmpty (_NW, _NE, _SW, _SE)
                then emptyTree qt.Region
                else makeTree qt.Region (Nodes(_NW, _NE, _SW, _SE))

        let treeQT = loop mtx.tree
        SparseMtx(treeQT, outerTypeOps)

    /// Converts sparse matrix to Array2D
    let toArray2D (mtx: SparseMtx<_>) =
        let resArray = Array2D.create mtx.size mtx.size (mtx.ops.getZero())
        iteri (fun x y elem -> resArray.[x, y] <- elem) mtx
        resArray

    /// Returns matrix of 2X size
    let doubleSize (mtx: SparseMtx<_>) =
        let reg = mtx.tree.Region :?> MatrixCell
        let newRegion = MatrixCell (reg.size * 2)
        let subs = MutableQT.toSubregions newRegion
        
        let _NE, _SW, _SE =
            emptyTree subs.NE,
            emptyTree subs.SW,
            emptyTree subs.SE
        
        let _NW = makeTree mtx.tree.Region mtx.tree.Content 
        let newTree = Nodes(_NW, _NE, _SW, _SE) |> makeTree newRegion
        SparseMtx(newTree, mtx.ops)
        
    /// Equality of two sparse matrices
    let isEqual (mtx1: SparseMtx<'a>) (mtx2: SparseMtx<'a>) =
        let cond1 = mtx1.ops.EqualToZero (mtx2.ops.getZero())
        let cond2 = mtx2.ops.EqualToZero (mtx1.ops.getZero())
        if not (cond1 && cond2)
        then failwith "Impossible to equalize matrices with different operators"
        
        let rec _go qt1 qt2 =
            match qt1.Content, qt2.Content with
            | Empty, Empty -> true
            | Empty, _ | _, Empty -> false
            | Leaf (p1, v1), Leaf (p2, v2) -> p1 = p2 && (mtx1.ops.equal v1 v2)
            | Nodes (nw1, ne1, sw1, se1), Nodes (nw2, ne2, sw2, se2) ->
                _go nw1 nw2
                && _go ne1 ne2
                && _go sw1 sw2
                && _go se1 se2
            | Leaf (p1, v1), Nodes (nw2, ne2, sw2, se2) ->
                let nw1, ne1, sw1, se1 = MutableQT.subdivideLeaf (p1, v1) qt1
                _go nw1 nw2
                && _go ne1 ne2
                && _go sw1 sw2
                && _go se1 se2
            | Nodes (nw1, ne1, sw1, se1), Leaf (p2, v2) ->
                let nw2, ne2, sw2, se2 = MutableQT.subdivideLeaf (p2, v2) qt2
                _go nw1 nw2
                && _go ne1 ne2
                && _go sw1 sw2
                && _go se1 se2
            
        match mtx1.size <> mtx2.size with
        | true -> false
        | false -> _go mtx1.tree mtx2.tree