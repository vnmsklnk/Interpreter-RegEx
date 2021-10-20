module MatrixLib.SparseArray2D

open System
open System.Threading.Tasks

module SparseArray2D =
    let getBounds (matrix: 'a [,]) =
        struct {| Rows = Array2D.length1 matrix
                  Cols = Array2D.length2 matrix |}

    /// Creates random integer sparse matrix
    let genIntMatrix (rows, cols) sparsity =
        if (rows <= 0 || cols <= 0) then
            failwith "Invalid matrix sizes"

        if (sparsity < 0.0 || sparsity > 1.0) then
            failwith "Invalid sparsity"

        let rand = Random()
        let result = Array2D.zeroCreate rows cols

        for j = 0 to rows - 1 do
            for k = 0 to cols - 1 do
                let y = rand.NextDouble()
                if (y > sparsity)
                then result.[j, k] <- rand.Next()
                else result.[j, k] <- 0

        result

    /// Adds two sparse matrices together
    let add (sr: Semiring<'a>) (matrix1: 'a [,]) (matrix2: 'a [,]) =
        let s1, s2 = getBounds matrix1, getBounds matrix2

        let sizesAreEqual =
            s1.Rows = s2.Rows
            && s1.Cols = s2.Cols

        if not sizesAreEqual then
            "Matrices has different sizes:"
                + $" [{s1.Rows}x{s1.Cols}], [{s2.Rows}x{s2.Cols}]"
                    |> failwith

        matrix2
        |> Array2D.iteri
            (fun i j fromSnd ->
                matrix1.[i, j] <-
                    sr.add matrix1.[i, j] fromSnd)

        matrix1

    /// Multiplies two sparse matrices
    let multiply (sr: Semiring<'a>) (matrix1: 'a [,]) (matrix2: 'a [,]) =
        let s1, s2 = getBounds matrix1, getBounds matrix2

        // check multiplication condition
        if s1.Cols <> s2.Rows then
            "Incorrect sizes of matrices:"
                + $" {s1.Rows}x{s1.Cols}, {s2.Rows}x{s2.Cols}"
                    |> failwith

        let result =
            sr.zero ()
                |> Array2D.create
                    s1.Rows s2.Cols

        let _loop =
            Parallel.For(0, s1.Rows,
                (fun i ->
                    for j in 0 .. s2.Cols - 1 do
                        for k in 0 .. s1.Cols - 1 do
                            result.[i, j] <-
                                sr.add result.[i, j]
                                <| sr.mul matrix1.[i, k] matrix2.[k, j]))
        
        // return multiplication result
        result

    /// Kronecker product of two sparse matrices
    let kroneckerProduct (sr: Semiring<'a>) (matrix1: 'a [,]) (matrix2: 'a [,]) =
        let s1, s2 = getBounds matrix1, getBounds matrix2
        let rows = s1.Rows * s2.Rows
        let cols = s1.Cols * s2.Cols

        let result =
            Array2D.create
                rows cols (sr.zero ())

        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                result.[i, j] <-
                    sr.mul
                        matrix1.[i / s2.Rows, j / s2.Cols]
                        matrix2.[i % s2.Rows, j % s2.Cols]

        result

    
    let closure (sr: Semiring<'a>) predicateFunc (matrix: 'a [,]) =
        let countWhich cond (mat: 'a [,]) =
            let mutable counter = 0
            let countIf x = if cond x then counter <- counter + 1
            Array2D.iter countIf mat
            counter

        let mutable result = matrix
        let mutable _continue = true

        while _continue do
            let prev = countWhich predicateFunc result
            let multiplied = multiply sr result result
            let current =
                countWhich predicateFunc
                <| (add sr result multiplied)

            if prev = current then
                _continue <- false

        result
