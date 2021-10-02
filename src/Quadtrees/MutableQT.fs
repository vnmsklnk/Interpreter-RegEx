namespace Quadtrees.MutableQT

open Quadtrees.QuadtreeTypes // Region, Direction

/// Point-Region Quadtree implementation
/// (where region is two-dimensional space e.g. square, rectangle)
type Quadtree<'t> =
    { Region: Region
      mutable Content: QuadNode<'t> }
/// Node of Point-Region Quadtree
and QuadNode<'t> =
    | Empty
    | Leaf of point: (float * float) * value: 't
    | Nodes of NW: Quadtree<'t> * NE: Quadtree<'t> * SW: Quadtree<'t> * SE: Quadtree<'t>

[<AutoOpen>]
module QuadtreeUtils =
    /// Returns empty quadtree with specified region
    let emptyTree (region: Region) =
        { Region = region
          Content = Empty }
        
    /// Quadtree constructor function
    let makeTree (region: Region) content =
        { Region = region
          Content = content }

    /// Checks if all passed nodes are empty
    let allNodesEmpty nodes =
        match nodes with
        | Empty, Empty, Empty, Empty -> true
        | _ -> false

    /// Checks if all passed subtrees are empty
    let allSubtreesEmpty subtrees =
        let nw, ne, sw, se = subtrees
        match nw.Content, ne.Content, sw.Content, se.Content with
        | Empty, Empty, Empty, Empty -> true
        | _ -> false

/// Point-Region Quadtree Functions
module MutableQT =
    let private toSubregions (region: Region) =
        {| NW = region.Split NW
           NE = region.Split NE
           SW = region.Split SW
           SE = region.Split SE |}

    let private getNextTree dir (nodes: Quadtree<_> * Quadtree<_> * Quadtree<_> * Quadtree<_>) =
        let nw, ne, sw, se = nodes
        match dir with
        | NW -> nw
        | NE -> ne
        | SW -> sw
        | SE -> se

    /// Creates subtree for a quadtree, fills subtree with specified content
    /// The subtree is placed in the specified direction
    let private createSubtree dir content tree =
        { Region = tree.Region.Split dir
          Content = content }

    /// Balance tree containing single leaf; returns balanced subtree
    let balanceLeaf content (parent: Quadtree<_>) =
        let point, value = content
        match (parent.Region.SplitPossible()) with
        | false -> Error "Failed to balance leaf; Region could not be split."
        | true ->
            let dir =
                parent.Region.GetDirection point
            
            let leaf =
                { Region = parent.Region.Split dir
                  Content = (Leaf (point, value)) }
            
            let regions = toSubregions parent.Region
            // make empty subtrees with correct subregions
            let nw, ne, sw, se =
                { Region = regions.NW; Content = Empty },
                { Region = regions.NE; Content = Empty },
                { Region = regions.SW; Content = Empty },
                { Region = regions.SE; Content = Empty }

            // place leaf in the right direction
            let nodes =
                match dir with
                | NW -> (leaf, ne, sw, se)
                | NE -> (nw, leaf, sw, se)
                | SW -> (nw, ne, leaf, se)
                | SE -> (nw, ne, sw, leaf)

            // return balanced subtree containing leaf
            Ok nodes

    let private subdivide (dir: Direction) (parentTree: Quadtree<_>) =
        match parentTree.Content with
        | Nodes (nw, ne, sw, se) ->
            // if nodes exist - return an existing node in the specified direction
            getNextTree dir (nw, ne, sw, se)
        | Leaf (point, value) ->
            // if there is a leaf - we need to balance leaf first
            // balance leaf = calculate regions and create empty nodes
            match balanceLeaf (point, value) parentTree with
            | Error msg -> failwith $"Failed to subdivide; {msg}"
            | Ok balanced ->
                let nw, ne, sw, se = balanced
                // transform current node to subtree of nodes
                parentTree.Content <- Nodes(nw, ne, sw, se)
                // then continue traversing in specified direction
                getNextTree dir balanced
        | Empty ->
            // if there is an empty tree - just subdivide regions,
            // then assign every region to subtree
            let nw, ne, sw, se as nodes =
                createSubtree NW Empty parentTree,
                createSubtree NE Empty parentTree,
                createSubtree SW Empty parentTree,
                createSubtree SE Empty parentTree
            // save subtrees at current node
            parentTree.Content <- Nodes(nw, ne, sw, se)
            getNextTree dir nodes

    /// Returns option of value at (x, y) (FLOAT) coordinates in the quadtree.
    /// If coordinates are outside the region - throws ArgumentException.
    /// If no point with specified coordinates exists in the tree - returns None
    let get (x, y) (tree: Quadtree<_>) =
        // throw ArgumentException exception if coordinates is incorrect
        if not (tree.Region.Contains(x, y)) then
            $"Given coordinates is out of range; region [{tree.Region.SizeX} x {tree.Region.SizeY}]"
            |> invalidArg "(x, y)"

        // returns None if nothing found
        let rec _go (qt: Quadtree<_>) =
            match qt.Content with
            | Empty -> None
            | Leaf (coord, value) ->
                match coord = (x, y) with
                | true -> Some value
                | false -> None
            | Nodes (nw, ne, sw, se) ->
                (nw, ne, sw, se)
                |> getNextTree (qt.Region.GetDirection(x, y))
                |> _go

        // search in tree by point coordinates
        _go tree

    /// Returns option of value at (x, y) (INTEGER) coordinates in the quadtree.
    /// If coordinates are outside the region - throws ArgumentException.
    /// If no point with specified coordinates exists in the tree - returns None
    let get' (x, y) tree =
        // convert int coordinates
        let x, y = (float x, float y)
        get (x, y) tree

    /// Inserts a point (with associated value) into a quadtree.
    /// Returns true if point was inserted in quadtree otherwise false
    let rec insert (x, y) newValue (tree: Quadtree<_>) =
        match tree.Region.Contains(x, y) with
        | false -> false
        | true ->
            match tree.Content with
            | Empty ->
                tree.Content <- Leaf((x, y), newValue)
                true
            | Leaf (coord, _) when coord = (x, y) ->
                tree.Content <- Leaf((x, y), newValue)
                true
            | Leaf _ ->
                subdivide (tree.Region.GetDirection (x, y)) tree
                |> insert (x, y) newValue
            | Nodes (nw, ne, sw, se) ->
                (nw, ne, sw, se)
                |> getNextTree (tree.Region.GetDirection(x, y))
                |> insert (x, y) newValue

    /// Sets a point with specified value by integer coordinates in a Quadtree.
    /// Returns true if point was set successfully otherwise false
    let set (x: int, y: int) value tree =
        let X, Y = float x, float y
        insert (X, Y) (value: 'value) tree

    /// Applies the given function to each element of the array.
    let rec iter action (tree: Quadtree<_>) =
        match tree.Content with
        | Empty -> ()
        | Leaf (_, value) -> action value
        | Nodes (nw, ne, sw, se) ->
            iter action nw
            iter action ne
            iter action sw
            iter action se

    /// Applies the given func. to value at each existing point in the quadtree.
    /// The FLOAT indices passed to the func. are the coordinates of point.
    let rec iteri action tree =
        match tree.Content with
        | Empty -> ()
        | Leaf (point, v) -> action (fst point) (snd point) v
        | Nodes (nw, ne, sw, se) ->
            iteri action nw
            iteri action ne
            iteri action sw
            iteri action se

    /// Applies the given func. to value at each existing point in the quadtree.
    /// The INTEGER indices are the coordinates of point.
    let rec iteri' (action: int -> int -> 'a -> unit) tree =
        match tree.Content with
        | Empty -> ()
        | Leaf ((x, y), v) -> action (int x) (int y) v
        | Nodes (nw, ne, sw, se) ->
            iteri' action nw
            iteri' action ne
            iteri' action sw
            iteri' action se

    /// Element-wise (point-wise) sum of points contained in two quadtrees
    let rec sum (identity, addition) (treeA: Quadtree<_>) (treeB: Quadtree<_>) =
        let Xa, Ya = treeA.Region.SizeX, treeA.Region.SizeY
        let Xb, Yb = treeB.Region.SizeX, treeB.Region.SizeY

        // throw ArgumentException exception
        // if threes dimensions differ
        if not ((Xa = Xb) && (Ya = Yb)) then
            $"Quadtrees with different sizes: {Xa}x{Ya} != {Xb}x{Yb}, could not be added together"
            |> invalidArg "treeB"

        match treeA.Content, treeB.Content with
        | Empty, contentB ->
            { Region = treeA.Region
              Content = contentB }
        | contentA, Empty ->
            { Region = treeA.Region
              Content = contentA }
        | Leaf (_, a), Leaf (point, b) ->
            let result = addition a b
            if result = identity then
                { Region = treeA.Region
                  Content = Empty }
            else
                { Region = treeA.Region
                  Content = (Leaf(point, result)) }

        | Leaf (pointA, a), Nodes (nw2, ne2, sw2, se2) ->
            match balanceLeaf (pointA, a) treeA with
            | Error msg -> failwith $"Failed to sum subtrees: {Xa}x{Ya}, {Xb}x{Yb}; {msg}"
            | Ok (nw, ne, sw, se) ->
                let _nw = sum (identity, addition) nw nw2
                let _ne = sum (identity, addition) ne ne2
                let _sw = sum (identity, addition) sw sw2
                let _se = sum (identity, addition) se se2
                
                if allSubtreesEmpty (_nw, _ne, _sw, _se) then
                    { Region = treeB.Region
                      Content = Empty }
                else
                    { Region = treeB.Region
                      Content = Nodes(_nw, _ne, _sw, _se) }

        | Nodes (nw1, ne1, sw1, se1), Leaf (pointB, b) ->
            match balanceLeaf (pointB, b) treeB with
            | Error msg -> failwith $"Failed to sum subtrees: {Xa}x{Ya}, {Xb}x{Yb}; {msg}"
            | Ok (nw, ne, sw, se) ->
                let _nw = sum (identity, addition) nw1 nw
                let _ne = sum (identity, addition) ne1 ne
                let _sw = sum (identity, addition) sw1 sw
                let _se = sum (identity, addition) se1 se
                
                if allSubtreesEmpty (_nw, _ne, _sw, _se) then
                    { Region = treeA.Region
                      Content = Empty }
                else
                    { Region = treeA.Region
                      Content = Nodes(_nw, _ne, _sw, _se) }

        | Nodes (nw1, ne1, sw1, se1), Nodes (nw2, ne2, sw2, se2) ->
            let _nw = sum (identity, addition) nw1 nw2
            let _ne = sum (identity, addition) ne1 ne2
            let _sw = sum (identity, addition) sw1 sw2
            let _se = sum (identity, addition) se1 se2

            if allSubtreesEmpty (_nw, _ne, _sw, _se) then
                { Region = treeA.Region
                  Content = Empty }
            else
                { Region = treeA.Region
                  Content = Nodes(_nw, _ne, _sw, _se) }