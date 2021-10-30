namespace Quadtrees.MutableQT

open Quadtrees.QtTypes
open Direction
open Region

/// Generic implementation of Point-Region Quadtree Data Structure
/// - Region is an abstraction (immutable) of two-dimensional space;
/// region may have integer, float or custom coordinates
/// (implements `IRegion< 'coordinateType >` interface)
/// - Content represents quad node data which could be:
///     Emtpy (no data inside);
///     Leaf (coordinates point (point in 2D space) associated with value of type 'a);
///     Nodes (sub-nodes inside the quadtree);
/// content of quadtree is mutable and could
/// be changed explicitly or by inserting new values
type Quadtree<'c, 't> =
    { Region: IRegion<'c>
      mutable Content: QuadNode<'c, 't> }

and QuadNode<'c, 't> =
    | Empty
    | Leaf of point: ('c * 'c) * value: 't
    | Nodes of  NW: Quadtree<'c, 't> *
                NE: Quadtree<'c, 't> *
                SW: Quadtree<'c, 't> *
                SE: Quadtree<'c, 't>

[<AutoOpen>]
module QuadtreeUtils =
    /// Returns quadtree region size X,Y
    let getTreeBounds tree =
        tree.Region.SizeX,
        tree.Region.SizeY
    
    /// Returns empty quadtree with specified region
    let emptyTree (region: IRegion<_>) =
        { Region = region
          Content = Empty }
        
    /// Quadtree constructor function
    let makeTree (region: IRegion<_>) content =
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

/// Basic functions
/// (e.g. subdivide, iter, iteri, insert)
/// implemented for mutable quadtree
module MutableQT =
    let private getNextTree dir nodes: Quadtree<_,_> =
        let nw, ne, sw, se = nodes
        match dir with
        | NW -> nw
        | NE -> ne
        | SW -> sw
        | SE -> se
    
    /// Returns option of value at (x, y) coordinates in the quadtree.
    /// If coordinates are outside the region - throws ArgumentException.
    /// If no point with specified coordinates exists in the tree - returns None
    let get (x, y) (tree: Quadtree<_,_>) =
        // throw ArgumentException exception if coordinates is incorrect
        if not (tree.Region.Contains(x, y)) then
            $"Given coordinates is out of range; region [{tree.Region.SizeX} x {tree.Region.SizeY}]"
            |> invalidArg "(x, y)"

        // returns None if nothing found
        let rec _go (qt: Quadtree<_,_>) =
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
    
    let toSubregions (region: IRegion<_>) =
        {| NW = region.Split NW
           NE = region.Split NE
           SW = region.Split SW
           SE = region.Split SE |}
    
    /// Creates subtree for a quadtree, fills subtree with specified content
    /// The subtree is placed in the specified direction
    let private createSubtree dir content tree =
        { Region = tree.Region.Split dir
          Content = content }

    /// Balance tree containing single leaf; returns balanced subtree
    let balanceLeaf (point, value) (parent: Quadtree<'c,'a>) =
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

    let subdivide (dir: Direction) (parentTree: Quadtree<_,_>) =
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
    
    let subdivideLeaf leaf parentTree =
        match balanceLeaf leaf parentTree with
        | Error msg -> failwith $"Failed to subdivide; {msg}"
        | Ok balanced -> balanced
    
    /// Inserts a point (with associated value) into a quadtree.
    /// Returns true if point was inserted in quadtree otherwise false
    let rec insert (x, y) newValue (tree: Quadtree<_,_>) =
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
    
    /// Applies the given function to each element of the array.
    let rec iter action tree =
        match tree.Content with
        | Empty -> ()
        | Leaf (_, value) -> action value
        | Nodes (nw, ne, sw, se) ->
            iter action nw
            iter action ne
            iter action sw
            iter action se
    
    /// Applies the given func. to value at each existing point in the quadtree.
    /// The specified indices are coordinates of points
    let rec iteri action tree =
        match tree.Content with
        | Empty -> ()
        | Leaf ((x, y), value) -> action x y value
        | Nodes (nw, ne, sw, se) ->
            iteri action nw
            iteri action ne
            iteri action sw
            iteri action se
    
    // fsharplint:disable camelCase
    /// Point-wise sum of elements contained in two quadtrees
    let rec sum operators (treeA: Quadtree<'c,_>) (treeB: Quadtree<'c,_>) =
        let equalToZero, addition = operators
        let Xa, Ya = treeA.Region.SizeX, treeA.Region.SizeY
        let Xb, Yb = treeB.Region.SizeX, treeB.Region.SizeY

        // throw ArgumentException exception
        // if threes dimensions differ
        if not ((Xa = Xb) && (Ya = Yb)) then
            "Quadtrees with different sizes:"
                + $" {Xa}x{Ya} != {Xb}x{Yb}, could not be added together"
                    |> invalidArg "treeB"

        match treeA.Content, treeB.Content with
        | Empty, contentB -> makeTree treeA.Region contentB
        | contentA, Empty -> makeTree treeA.Region contentA
        | Leaf (_, a), Leaf (point, b) ->
            let res = addition a b
            if equalToZero res then emptyTree treeA.Region
            else Leaf(point, res) |> makeTree treeA.Region
        | Leaf (pointA, a), Nodes (nw2, ne2, sw2, se2) ->
            match balanceLeaf (pointA, a) treeA with
            | Error msg -> failwith $"Failed to sum subtrees: {Xa}x{Ya}, {Xb}x{Yb}; {msg}"
            | Ok (nw, ne, sw, se) ->
                let _nw = sum operators nw nw2
                let _ne = sum operators ne ne2
                let _sw = sum operators sw sw2
                let _se = sum operators se se2
                // if all subtrees are empty - parent tree should be empty
                if allSubtreesEmpty (_nw, _ne, _sw, _se) then emptyTree treeB.Region
                else Nodes(_nw, _ne, _sw, _se) |> makeTree treeB.Region

        | Nodes (nw1, ne1, sw1, se1), Leaf (pointB, b) ->
            match balanceLeaf (pointB, b) treeB with
            | Error msg -> failwith $"Failed to sum subtrees: {Xa}x{Ya}, {Xb}x{Yb}; {msg}"
            | Ok (nw, ne, sw, se) ->
                let _nw = sum operators nw1 nw
                let _ne = sum operators ne1 ne
                let _sw = sum operators sw1 sw
                let _se = sum operators se1 se
                // if all subtrees are empty - parent tree should also be empty
                if allSubtreesEmpty (_nw, _ne, _sw, _se) then emptyTree treeA.Region
                else Nodes(_nw, _ne, _sw, _se) |> makeTree treeA.Region

        | Nodes (nw1, ne1, sw1, se1), Nodes (nw2, ne2, sw2, se2) ->
            let _nw = sum operators nw1 nw2
            let _ne = sum operators ne1 ne2
            let _sw = sum operators sw1 sw2
            let _se = sum operators se1 se2
            // if all subtrees are empty - parent tree should also be empty
            if allSubtreesEmpty (_nw, _ne, _sw, _se) then emptyTree treeA.Region
            else Nodes(_nw, _ne, _sw, _se) |> makeTree treeA.Region
