module Quadtrees.UnitTests.MainTests

open System
open Expecto                        // Test Framework
open MatrixLib.AlgebraicStructures  // Semirings
open MatrixLib.SparseMatrixQT       // Sparse matrix functions
open MatrixLib.Utils                // Array2D functions for comparison

let tests =
    testSequenced <| testList "Math operations on quadtrees" [
        testProperty "Array 2D closure equals closure on QuadTrees" <| fun (size: int) ->
            let size = (Math.Abs size + 1) |> toNextPowerOfTwo
            let semiring =
                    { GetGenericZero = fun () -> 0
                      Addition = (+)
                      Multiplication = (*) }
            
            if 4 < size && size < 32 then
                let randMatrix = generateMatrix size size 0.5               
                let sparse = SparseMatrixQT(randMatrix, (fun () -> 0), (=))
                
                let expected = closureArr2D 0 (+) (*) (fun x -> x > 0) randMatrix
                let resSparse = SparseMath.closure semiring (fun x -> x > 0) sparse
                
                let actual = SparseMatrixQT.toArray2D 0 resSparse
                Expect.equal actual expected ""
        let semiring =
                    { GetGenericZero = fun () -> 0
                      Addition = (+)
                      Multiplication = (*) }
                    
        testCase "Closure testCase" <| fun () ->
            let matrix = array2D [|
                    [|0; 1; 564; 5|]
                    [|1; 28; 1; 6|]
                    [|1; 5; 1; 8|]
                    [|1; 8; 1; 6|]
                |]
            
            let matrix' = array2D [|
                    [|0; 1; 564; 5|]
                    [|1; 28; 1; 6|]
                    [|1; 5; 1; 8|]
                    [|1; 8; 1; 6|]
                |]
            
            let expected = closureArr2D 0 (+) (*) (fun x -> x > 0) matrix
            let sparse = SparseMatrixQT(matrix', (fun () -> 0), (=))
            //printfn $"%A{sparse.quadtree}"
            printfn $"%A{SparseMatrixQT.toArray2D 0 sparse}"
            
            //printfn $"FUCKING SLAAAAVE: %A{matrix}"
            //printfn $"MASTER??????: %A{expected}"
            let resSparse = SparseMath.closure semiring (fun x -> x > 0) sparse
            let actual = SparseMatrixQT.toArray2D 0 resSparse
            printfn $"Actual:\n %A{actual}"
            printfn $"Expected:\n %A{expected}"
            Expect.equal actual expected ""
            
        let helperMult (matrixA: int[,]) (matrixB: int[,]) =
            let sparseA = SparseMatrixQT(matrixA, (fun () -> 0), (=))
            let sparseB = SparseMatrixQT(matrixB, (fun () -> 0), (=))
            let expected = multiply 0 (+) (*) matrixA matrixB
            let resSparse = SparseMath.multiply semiring sparseA sparseB
            let actual = SparseMatrixQT.toArray2D 0 resSparse
            actual, expected
            
        let helperSum (matrixA: int[,]) (matrixB: int[,]) =
            let sparseA = SparseMatrixQT(matrixA, (fun () -> 0), (=))
            let sparseB = SparseMatrixQT(matrixB, (fun () -> 0), (=))
            let expected = add (+) matrixA matrixB
            let resSparse = SparseMath.sum semiring sparseA sparseB
            let actual = SparseMatrixQT.toArray2D 0 resSparse
            actual, expected
            
        let helperTensorMult (matrixA: int[,]) (matrixB: int[,]) =
            let sparseA = SparseMatrixQT(matrixA, (fun () -> 0), (=))
            let sparseB = SparseMatrixQT(matrixB, (fun () -> 0), (=))
            let expected = tensorMultiply (*) matrixA matrixB
            let resSparse = SparseMath.tensorMultiply' semiring sparseA sparseB
            let actual = SparseMatrixQT.toArray2D 0 resSparse
            actual, expected
            
        let helperClosure (matrix: int[,]) =
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let expected = closureArr2D 0 (+) (*) (fun _ -> true) matrix
            let resSparse = SparseMath.closure semiring (fun _ -> true) sparse
            let actual = SparseMatrixQT.toArray2D 0 resSparse
            actual, expected
        
        let matrixA = array2D [|
                    [|5; 0; 4; 6|]
                    [|0; 28; 7; 0|]
                    [|0; 5; 1; 0|]
                    [|0; 0; 1; 7|]
                |]
        let matrixB = array2D [|
                    [|7; 0; 0; 6|]
                    [|0; 8; 0; 0|]
                    [|0; 5; 2; 0|]
                    [|8; 0; 1; 7|]
                |]
        
        let matrixC = array2D [|
                    [|7; 0; 0; 6|]
                    [|0; 0; 7; 0|]
                    [|0; 0; 5; 0|]
                    [|0; 0; 1; 7|]
                |]
        let matrixD = array2D [|
                    [|4; 0; 0; 6|]
                    [|0; 8; 5; 0|]
                    [|0; 0; 2; 0|]
                    [|8; 0; 1; 8|]
                |]
        let matrixE = array2D [|
                    [|7; 0|]
                    [|0; 0|]
                |]
        let matrixF = array2D [|
                    [|4; 0|]
                    [|0; 8|]
                |]
        let matrixG = array2D [|
                    [|7; 1; 8; 9; 2; 5; 9; 18|]
                    [|6; 0; 8; 9; 2; 5; 4; 1|]
                    [|7; 0; 5; 9; 2; 4; 92; 12|]
                    [|7; 0; 8; 9; 2; 5; 9; 1|]
                    [|0; 0; 0; 0; 2; 5; 9; 5|]
                    [|0; 0; 8; 0; 9; 59; 19; 15|]
                    [|0; 0; 0; 0; 2; 5; 9; 19|]
                    [|0; 0; 0; 0; 2; 5; 9; 12|]
                |]
        let matrixH = array2D [|
                    [|0; 0; 0; 0; 2; 5; 9; 18|]
                    [|0; 0; 0; 9; 2; 5; 4; 1|]
                    [|0; 0; 0; 0; 2; 4; 7; 16|]
                    [|0; 0; 8; 0; 67; 5; 12; 1|]
                    [|6; 0; 0; 0; 2; 5; 9; 5|]
                    [|0; 0; 8; 0; 9; 4; 19; 11|]
                    [|0; 9; 0; 0; 6; 4; 9; 9|]
                    [|0; 0; 7; 0; 2; 4; 9; 1|]
                |]
        
        testCase "Sum test 1" <| fun () ->
            let actual, expected = helperSum matrixA matrixB
            Expect.equal actual expected ""
        testCase "Sum test 2" <| fun () ->
            let actual, expected = helperSum matrixC matrixD
            Expect.equal actual expected ""
        testCase "Sum test 3" <| fun () ->
            let actual, expected = helperSum matrixE matrixF
            Expect.equal actual expected ""
        testCase "Sum test 4" <| fun () ->
            let actual, expected = helperSum matrixG matrixH
            Expect.equal actual expected ""
            
        testCase "Tensor test 1" <| fun () ->
            let actual, expected = helperTensorMult matrixA matrixB
            Expect.equal actual expected ""
        testCase "Tensor test 2" <| fun () ->
            let actual, expected = helperTensorMult matrixC matrixD
            Expect.equal actual expected ""
        testCase "Tensor test 3" <| fun () ->
            let actual, expected = helperTensorMult matrixE matrixF
            Expect.equal actual expected ""
        testCase "Tensor test 4" <| fun () ->
            let actual, expected = helperTensorMult matrixG matrixH
            Expect.equal actual expected ""
            
        testCase "Multiply test 1" <| fun () ->
            let matrixA = array2D [|
                    [|7; 0|]
                    [|0; 0|]
                |]
            let matrixB = array2D [|
                    [|4; 0|]
                    [|0; 8|]
                |]
            let sparseA = SparseMatrixQT(matrixA, (fun () -> 0), (=))
            let sparseB = SparseMatrixQT(matrixB, (fun () -> 0), (=))
            let expected = multiply 0 (+) (*) matrixA matrixB
            let resSparse = SparseMath.multiply semiring sparseA sparseB
            
            let actual = SparseMatrixQT.toArray2D 0 resSparse
            Expect.equal actual expected ""
        testCase "Multiply test 2" <| fun () ->            
            let actual, expected = helperMult matrixC matrixD
            Expect.equal actual expected ""    
        testCase "Multiply test 3" <| fun () ->            
            let actual, expected = helperMult matrixE matrixF
            Expect.equal actual expected ""                                    
        testCase "Multiply test 4" <| fun () ->            
            let actual, expected = helperMult matrixG matrixH
            Expect.equal actual expected ""
            
        testCase "Init test 1" <| fun () ->
            let matrix = array2D [|
                    [|5; 0; 4; 6|]
                    [|0; 28; 7; 0|]
                    [|0; 5; 1; 0|]
                    [|0; 0; 1; 7|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""
        
        testCase "Init test 2" <| fun () ->
            let matrix = array2D [|
                    [|7; 1;|]
                    [|0; 8;|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""
        
        testCase "Init test 3" <| fun () ->
            let matrix = array2D [|
                    [|7; 0; 0; 6|]
                    [|0; 0; 7; 0|]
                    [|0; 0; 5; 0|]
                    [|0; 0; 1; 7|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""
        
        testCase "Init test 4" <| fun () ->
            let matrix = array2D [|
                    [|4; 0; 0; 6|]
                    [|0; 8; 5; 0|]
                    [|0; 0; 2; 0|]
                    [|8; 0; 1; 8|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""
        
        testCase "Init test 5" <| fun () ->
            let matrix = array2D [|
                    [|7; 0|]
                    [|0; 0|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""
        
        testCase "Init test 6" <| fun () ->
            let matrix = array2D [|
                    [|4; 0|]
                    [|0; 8|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""
        
        testCase "Init test 7" <| fun () ->
            let matrix = array2D [|
                    [|7; 1; 8; 9; 2; 5; 9; 18|]
                    [|6; 0; 8; 9; 2; 5; 4; 1|]
                    [|7; 0; 5; 9; 2; 4; 92; 12|]
                    [|7; 0; 8; 9; 2; 5; 9; 1|]
                    [|0; 0; 0; 0; 2; 5; 9; 5|]
                    [|0; 0; 8; 0; 9; 59; 19; 15|]
                    [|0; 0; 0; 0; 2; 5; 9; 19|]
                    [|0; 0; 0; 0; 2; 5; 9; 12|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""
        
        testCase "Init test 8" <| fun () ->
            let matrix = array2D [|
                    [|0; 0; 0; 0; 2; 5; 9; 18|]
                    [|0; 0; 0; 9; 2; 5; 4; 1|]
                    [|0; 0; 0; 0; 2; 4; 7; 16|]
                    [|0; 0; 8; 0; 67; 5; 12; 1|]
                    [|6; 0; 0; 0; 2; 5; 9; 5|]
                    [|0; 0; 8; 0; 9; 4; 19; 11|]
                    [|0; 9; 0; 0; 6; 4; 9; 9|]
                    [|0; 0; 7; 0; 2; 4; 9; 1|]
                |]
            let sparse = SparseMatrixQT(matrix, (fun () -> 0), (=))
            let result = SparseMatrixQT.toArray2D 0 sparse
            Expect.equal matrix result ""
                       
    ]
