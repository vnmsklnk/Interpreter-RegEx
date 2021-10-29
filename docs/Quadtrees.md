# Quadtrees

Quadtrees is a library of operations on quadtrees which is used in Matrix Library.

## QuadtreeUtils

QuadtreeUtils is a module with useful utilities for working with quadtrees.

* `getTreeBounds (tree: Quadtree<'a, 'b>` - returns quadtree region size X, Y.
* `emptyTree (region: IRegion<_>)` - returns empty quadtree with specified region.
* `makeTree (region: IRegion<_>) (content: QuadNode<'a, 'b>)` - construct quadtree from Quadnode.
* `allNodesEmpty (nodes: QuadNode<'a,'b> * QuadNode<'c,'d> * QuadNode<'e,'f> * QuadNode<'g,'h>)` - checks if all passed subtrees are empty.

## MutableQT

Basic operations for quadtrees.

* `get ((x: 'a) * (y: 'a)) (tree: Quadtree<'a,'b>)` - returns option of value at (x, y) coordinates in the quadtree.
* `balanceLeaf (point: ('c * 'c), value: 'a) (parent: Quadtree<'c,'a>)` - balance tree containing single leaf; returns balanced subtree.
* `insert (x: 'a, y: 'a) (newValue: 'b) (tree: Quadtree<_,_>)` - inserts a point (with associated value) into a quadtree.
* `iter (action: 'a -> unit) (tree: Quadtree<'b,'a>)` - applies the given func to each element of tree.
* `iteri (action: 'a -> -> 'a -> 'b -> unit) (tree: Quadtree<'a,'b>)` - applies the given func to each element of tree. The numbers passed to the function indicates the coordinates of element.
* `sum (operators: ('a -> bool) * ('a -> 'a -> 'a)) (tree1: Quadtree<'c,'a>) (tree2: Quadtree<'c,'a>)` - point-wise sum of elements contained in two quadtrees.
