module MatrixLib.Utils

open System                 // Math functions
open System.Numerics        // BitOperations function `LeadingZeroCount`
open System.Threading.Tasks // using Parallel.For()

/// Rounding an integer to the next nearest power of 2
let toNextPowerOfTwo number =
    let shift =
        number
        |> float
        |> Math.Log2
        |> Math.Ceiling

    1 <<< int shift

/// Fast rounding an integer to the next nearest power of 2 (incompatible with netstandard)
(* WARNING:
    - this function uses `BitOperations.LeadingZeroCount()` (netcore >= 3.1)
    - `System.Numerics.BitOperations` not supported by CLS (incompatible with netstandard) *)
let fastToNextPowerOfTwo (number: int) =
    let shift =
        sizeof<uint> * 8 - BitOperations.LeadingZeroCount(
            (uint number) - 1u)

    int (1u <<< shift)

[<AutoOpen>]
module Array2DWrapper =
    let generateMatrix rows cols sparsity =
        if (rows <= 0 || cols <= 0) then failwith "Invalid matrix sizes"
        if (sparsity < 0.0 || sparsity > 1.0) then failwith "Invalid sparsity"
        let rand = Random()
        let output = Array2D.zeroCreate rows cols
        for j = 0 to rows - 1 do
            for k = 0 to cols - 1 do
                let y = rand.NextDouble()
                if (y > sparsity) then output.[j, k] <- rand.Next()
                else output.[j, k] <- 0

        output

    let add (addOp: 'a -> 'a -> 'a) (matrix1: 'a [,]) (matrix2: 'a [,]) =
        let sizesAreEqual =
            matrix1.GetLength 0 = matrix2.GetLength 0
            && matrix1.GetLength 1 = matrix2.GetLength 1

        if not sizesAreEqual then
            failwith "Incorrect size of input matrices."

        matrix2
        |> Array2D.iteri (
            fun i j elemMatrix2 ->
                matrix1.[i, j] <- addOp matrix1.[i, j] elemMatrix2
            ) 

        matrix1

    let multiply genericZero (addOp: 'a -> 'a -> 'a) (mulOp: 'a -> 'a -> 'a) (matrix1: 'a [,]) (matrix2: 'a [,]) =
        let sizes1 =
            {| Rows = matrix1.GetLength 0
               Cols = matrix1.GetLength 1 |}

        let sizes2 =
            {| Rows = matrix2.GetLength 0
               Cols = matrix2.GetLength 1 |}

        // check matrix size
        if sizes1.Cols <> sizes2.Rows then
            failwith $"Incorrect size of input matrices: {sizes1.Rows}x{sizes1.Cols}, {sizes2.Rows}x{sizes2.Cols}"

        let result =
            genericZero
            |> Array2D.create sizes1.Rows sizes2.Cols 

        let _loop =
            Parallel.For(0, sizes1.Rows,
                (fun i ->
                    for j in 0 .. sizes2.Cols - 1 do
                        for k in 0 .. sizes1.Cols - 1 do
                            result.[i, j] <-
                                addOp result.[i, j]
                                <| mulOp matrix1.[i, k] matrix2.[k, j])
            )

        result

    let closureArr2D genericZero addOp mulOp predicateFunc (matrix: 'a [,]) =
        let countWhich cond (mat: 'a [,]) =
            let mutable cnt = 0
            Array2D.iter (fun x -> if cond x then cnt <- cnt + 1) mat
            cnt

        let mutable result = matrix
        let mutable _continue = true

        while _continue do
            let prev = countWhich predicateFunc result
            let multiplied = multiply genericZero addOp mulOp result result
            
            let current =
                countWhich predicateFunc
                <| (add addOp result multiplied)
            
            if prev = current then
                _continue <- false

        result
        
    let tensorMultiply (mulOp: 'a -> 'a -> 'a) (matrix1: 'a [,]) (matrix2: 'a [,]) =
        let rows = matrix1.GetLength 0 * matrix2.GetLength 0
        let cols = matrix1.GetLength 1 * matrix2.GetLength 1
        let result = Array2D.zeroCreate rows cols
        for i in 0..rows - 1 do
            for j in 0..cols - 1 do
                result.[i,j] <- mulOp
                    matrix1.[i / matrix2.GetLength 0, j / matrix2.GetLength 1]
                    matrix2.[i % matrix2.GetLength 0, j % matrix2.GetLength 0]
        result