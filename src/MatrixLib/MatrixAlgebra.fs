namespace MatrixLib
open Quadtrees.QtTypes.MatrixCell
open Quadtrees.MutableQT
// fsharplint:disable camelCase
    
module MatrixAlgebra =
    let sum (sr: Semiring<_>) (mtx1: SparseMtx<_>) (mtx2: SparseMtx<_>) = 
        let res =
            MutableQT.sum
                (Structures.equalityToZero sr, sr.add) mtx1.tree mtx2.tree
        
        SparseMtx(res, Structures.toOps sr.zero sr.eq)

    let multiply (sr: Semiring<_>) (mtx1: SparseMtx<_>) (mtx2: SparseMtx<_>) =
        let ops = Structures.equalityToZero sr, sr.add
        // recursive multiply of square region quadtree
        let rec loop curr qt1 qt2 =
            /// Multiplication of subtrees
            let mulSubs parent nodes1 nodes2 =
                let NW1, NE1, SW1, SE1 = nodes1
                let NW2, NE2, SW2, SE2 = nodes2
                let cellNW, cellNE, cellSW, cellSE = getSubsParams parent
                let subtrees =
                    MutableQT.sum ops (loop cellNW NW1 NW2) (loop cellNW NE1 SW2),
                    MutableQT.sum ops (loop cellNE NW1 NE2) (loop cellNE NE1 SE2),                
                    MutableQT.sum ops (loop cellSW SW1 NW2) (loop cellSW SE1 SW2),
                    MutableQT.sum ops (loop cellSE SW1 NE2) (loop cellSE SE1 SE2)
                
                // if all subtrees are empty - parent tree should be empty
                if allSubtreesEmpty subtrees
                then emptyTree (MatrixCell parent) 
                else makeTree (MatrixCell parent) (Nodes subtrees)
            
            match qt1.Content, qt2.Content with
            | Empty, _ -> MatrixCell(curr.X, curr.Y, curr.Size) |> emptyTree
            | _, Empty -> MatrixCell(curr.X, curr.Y, curr.Size) |> emptyTree
            | Leaf (p1, v1), Nodes (nw2, ne2, sw2, se2) ->
                let nodes1 = MutableQT.subdivideLeaf (p1, v1) qt1
                let nodes2 = (nw2, ne2, sw2, se2)
                mulSubs curr nodes1 nodes2
            
            | Nodes (nw1, ne1, sw1, se1), Leaf (p2, v2) ->
                let nodes1 = (nw1, ne1, sw1, se1)
                let nodes2 = MutableQT.subdivideLeaf (p2, v2) qt2
                mulSubs curr nodes1 nodes2
            
            | Leaf ((x1, _), A), Leaf ((_, y2), B) when curr.Size = 1 ->
                let res = (sr.mul A B)
                if sr.EqualToZero res
                then emptyTree (MatrixCell curr)
                else Leaf ((x1, y2), res) |> makeTree (MatrixCell curr)
            
            | Leaf (point1, A), Leaf (point2, B) ->
                let nodes1 = MutableQT.subdivideLeaf (point1, A) qt1
                let nodes2 = MutableQT.subdivideLeaf (point2, B) qt2
                mulSubs curr nodes1 nodes2
            
            | Nodes (nw1, ne1, sw1, se1), Nodes (nw2, ne2, sw2, se2) ->
                let nodes1 = (nw1, ne1, sw1, se1)
                let nodes2 = (nw2, ne2, sw2, se2)
                mulSubs curr nodes1 nodes2

        // check regions                
        let cell1, cell2 =
            regionsAreCells
                mtx1.tree.Region
                mtx2.tree.Region
        
        let cell = cellsCanBeMultiplied cell1 cell2
        let resultTree = loop cell mtx1.tree mtx2.tree
        SparseMtx (resultTree, Operators(sr.zero, sr.eq))

    let multiplyParallel (sr: Semiring<_>) depth (mtx1: SparseMtx<_>) (mtx2: SparseMtx<_>) =
        let ops = Structures.equalityToZero sr, sr.add
        // recursive multiply of square region quadtree
        let rec loop (curr, count) qt1 qt2 =
            /// Parallel multiplication of subtrees
            let mulParallel (parent, count) nodes1 nodes2 =
                let NW1, NE1, SW1, SE1 = nodes1
                let NW2, NE2, SW2, SE2 = nodes2
                let cellNW, cellNE, cellSW, cellSE = getSubsParams parent
                // get multiplied subtrees
                let subtrees =
                    match count < depth with
                    | false ->
                        MutableQT.sum ops (loop (cellNW, count) NW1 NW2) (loop (cellNW, count) NE1 SW2),
                        MutableQT.sum ops (loop (cellNE, count) NW1 NE2) (loop (cellNE, count) NE1 SE2),                
                        MutableQT.sum ops (loop (cellSW, count) SW1 NW2) (loop (cellSW, count) SE1 SW2),
                        MutableQT.sum ops (loop (cellSE, count) SW1 NE2) (loop (cellSE, count) SE1 SE2)
                    | true ->
                        let cnt = count + 1
                        let computations = [
                          async { return MutableQT.sum ops (loop (cellNW, cnt) NW1 NW2) (loop (cellNW, cnt) NE1 SW2) }
                          async { return MutableQT.sum ops (loop (cellNE, cnt) NW1 NE2) (loop (cellNE, cnt) NE1 SE2) }                
                          async { return MutableQT.sum ops (loop (cellSW, cnt) SW1 NW2) (loop (cellSW, cnt) SE1 SW2) }
                          async { return MutableQT.sum ops (loop (cellSE, cnt) SW1 NE2) (loop (cellSE, cnt) SE1 SE2) }
                        ]
                        let res = Async.RunSynchronously <| Async.Parallel computations
                        // return computed subtrees
                        (res.[0], res.[1], res.[2], res.[3])
                
                // if all subtrees are empty - parent tree should be empty
                if allSubtreesEmpty subtrees 
                then emptyTree (MatrixCell parent) 
                else makeTree (MatrixCell parent) (Nodes subtrees)
            
            match qt1.Content, qt2.Content with
            | Empty, _ -> MatrixCell(curr.X, curr.Y, curr.Size) |> emptyTree
            | _, Empty -> MatrixCell(curr.X, curr.Y, curr.Size) |> emptyTree
            | Leaf (p1, v1), Nodes (nw2, ne2, sw2, se2) ->
                let nodes1 = MutableQT.subdivideLeaf (p1, v1) qt1
                let nodes2 = (nw2, ne2, sw2, se2)
                mulParallel (curr, count) nodes1 nodes2
            
            | Nodes (nw1, ne1, sw1, se1), Leaf (p2, v2) ->
                let nodes1 = (nw1, ne1, sw1, se1)
                let nodes2 = (MutableQT.subdivideLeaf (p2, v2) qt2)
                mulParallel (curr, count) nodes1 nodes2
            
            | Leaf ((x1, _), A), Leaf ((_, y2), B) when curr.Size = 1 ->
                let res = (sr.mul A B)
                if sr.EqualToZero res
                then emptyTree (MatrixCell curr)
                else Leaf ((x1, y2), res) |> makeTree (MatrixCell curr)
            
            | Leaf (point1, A), Leaf (point2, B) ->
                let nodes1 = MutableQT.subdivideLeaf (point1, A) qt1
                let nodes2 = MutableQT.subdivideLeaf (point2, B) qt2
                mulParallel (curr, count) nodes1 nodes2
            
            | Nodes (nw1, ne1, sw1, se1), Nodes (nw2, ne2, sw2, se2) ->
                let nodes1 = (nw1, ne1, sw1, se1)
                let nodes2 = (nw2, ne2, sw2, se2)
                mulParallel (curr, count) nodes1 nodes2
        
        // check regions
        let cell1, cell2 =
            regionsAreCells
                mtx1.tree.Region
                mtx2.tree.Region
        
        let cell = cellsCanBeMultiplied cell1 cell2
        let resultTree = loop (cell, 0) mtx1.tree mtx2.tree
        SparseMtx (resultTree, Structures.toOps sr.zero sr.eq)
    
    open Quadtrees.Utils
    
    let kroneckerProduct (sr: Semiring<_>) (mtxA: SparseMtx<_>) (mtxB: SparseMtx<_>) =
        let X1, Y1 = getTreeBounds mtxA.tree
        let X2, Y2 = getTreeBounds mtxB.tree
        
        let sizeX, sizeY = X1 * X2, Y1 * Y2
        let cntX, cntY = half sizeX, half sizeY
        let tree = MatrixCell(cntX, cntY, sizeX) |> emptyTree

        let rec loop treeA treeB =
            match treeA.Content, treeB.Content with
            | Leaf ((Xa, Ya), A), Leaf ((Xb, Yb), B) ->
                let coord = (Xa * mtxB.size) + Xb, (Ya * mtxB.size) + Yb
                let multiplied = sr.mul A B
                if sr.EqualToZero multiplied then ()
                else MutableQT.insert coord multiplied tree |> ignore
            | Leaf _, Nodes (y1, y2, y3, y4) ->
                loop treeA y1
                loop treeA y2
                loop treeA y3
                loop treeA y4
            | Nodes (x1, x2, x3, x4), _ ->
                loop x1 treeB
                loop x2 treeB
                loop x3 treeB
                loop x4 treeB
            | Empty, _ -> ()
            | _, Empty -> ()

        loop mtxA.tree mtxB.tree
        SparseMtx(tree, Operators(sr.zero, sr.eq))  
    
    /// Returns transitive closure of sparse matrix
    let closure sr (mtx: SparseMtx<_>) =
        let mutable prev = mtx
        let mutable result = mtx
        let mutable _continue = true

        while _continue do
            let multiplied = multiply sr result result           
            let added = (sum sr result multiplied)
            result <- added
            if SparseMtx.isEqual prev result then _continue <- false
            else prev <- result
        result
