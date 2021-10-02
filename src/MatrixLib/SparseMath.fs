namespace MatrixLib.SparseMatrixQT

open System.Collections.Generic
open FSharp.Core

open Quadtrees.QuadtreeTypes            // Square region
open Quadtrees.MutableQT                // using mutable QT impl.
open MatrixLib.AlgebraicStructures      // semirings

module SparseMath =
    let sum sr (mtx1: SparseMatrixQT<_>) (mtx2: SparseMatrixQT<_>) =
        let res =
            MutableQT.sum (sr.GetGenericZero(), sr.Addition)
                mtx1.quadtree mtx2.quadtree

        SparseMatrixQT(res, sr.GetGenericZero)

    let multiply sr (mtxA: SparseMatrixQT<_>) (mtxB: SparseMatrixQT<_>) =
        // check square matrix size
        if mtxA.size <> mtxB.size then
            failwith $"Incorrect size of input matrices: {mtxA.size}x{mtxA.size}, {mtxB.size}x{mtxB.size}"

        let mutable res =
            SparseMatrixQT(mtxA.size, sr.GetGenericZero)

        for x in 0 .. mtxA.size - 1 do
            for y in 0 .. mtxB.size - 1 do
                let mutable acc = sr.GetGenericZero()
                for z in 0 .. mtxA.size - 1 do
                    acc <- sr.Addition acc (sr.Multiplication mtxA.[x, z] mtxB.[z, y])
                res.[x, y] <- acc

        res


    let tensorMultiply' semiring (mtxA: SparseMatrixQT<_>) (mtxB: SparseMatrixQT<_>) =
        let newLeafCoord (X: float, Y: float) delta pointB =
            let Xb, Yb = pointB
            ((X * delta) + Xb, (Y * delta) + Yb)

        let X1, Y1 =
            mtxA.quadtree.Region.SizeX,
            mtxA.quadtree.Region.SizeY

        let X2, Y2 =
            mtxB.quadtree.Region.SizeX,
            mtxB.quadtree.Region.SizeY

        let x, y = (X1 * X2) / 2.0, (Y1 * Y2) / 2.0
        let SizeX = X1 * X2
        let _SizeY = Y1 * Y2
        let resTree = emptyTree (Square(x, y, SizeX))
        
        
        let rec _go (treeA: Quadtree<_>) (treeB: Quadtree<_>) =
            match treeA.Content, treeB.Content with
            | Leaf (pointA, a), Leaf (pointB, b) ->
                let fixedCoord =
                    newLeafCoord pointA (float mtxB.size) pointB
                let res = semiring.Multiplication a b
                if res <> semiring.GetGenericZero()
                then MutableQT.insert fixedCoord res resTree |> ignore
                else ()
            | Leaf _, Nodes (y1, y2, y3, y4) ->
                _go treeA y1
                _go treeA y2
                _go treeA y3
                _go treeA y4
            | Nodes (x1, x2, x3, x4), _ ->
                _go x1 treeB
                _go x2 treeB
                _go x3 treeB
                _go x4 treeB
            | Empty, _ -> ()
            | _, Empty -> ()

        _go mtxA.quadtree mtxB.quadtree
        SparseMatrixQT(resTree, semiring.GetGenericZero)
        
    let tensorMultiply semiring (mtxA: SparseMatrixQT<HashSet<_>>) (mtxB: SparseMatrixQT<HashSet<_>>) =
        let multiplicationSets (s1:HashSet<_>) (s2: HashSet<_>) =
            let res = HashSet<_>(s1) in res.IntersectWith s2
            res

        let X1, Y1 =
            mtxA.quadtree.Region.SizeX,
            mtxA.quadtree.Region.SizeY
        
        let X2, Y2 =
            mtxB.quadtree.Region.SizeX,
            mtxB.quadtree.Region.SizeY

        let rows = int (X1 * X2)
        let columns = int (Y1 * Y2)
        let mutable res = SparseMatrixQT(emptyTree (Square(rows)), semiring.GetGenericZero)
        for i in 0..rows - 1 do
            for j in 0..columns - 1 do
                let first = mtxA.[i / mtxB.size, j / mtxB.size ]
                let second = mtxB.[i % mtxB.size, j % mtxB.size]
                let kek = multiplicationSets first second
                res.[i, j] <- kek
        res
        


    let closure sr condition (mtx: SparseMatrixQT<_>) =
        let count cond (mtx': SparseMatrixQT<_>) =
            let mutable count = 0

            mtx'
            |> SparseMatrixQT.iter
                (fun elem ->
                    if cond elem then
                        count <- count + 1)

            count

        let mutable result = mtx
        let mutable _continue = true

        while _continue do
            let prev = count condition result
            let multiplied = multiply sr result result
            
            let added = (sum sr result multiplied)
            result <- added
            let current =
                count condition added
            
            if prev = current then
                _continue <- false

        result