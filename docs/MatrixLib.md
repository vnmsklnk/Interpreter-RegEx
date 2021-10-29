# Matrix Library

MatrixLib is a quadtrees based library for working with matrices.

## Matrix Algebra

Matrix Algebra is a module with algebraic operations on matrices.

*	`sum (semiring: Semiring<_>) (matrix1: SparseMtx<_>) (matrix2: SparseMtx<_>)` - adds two matrices.
*	`multiply (semiring: Semiring<_>) (matrix1: SparseMtx<_>) (matrix2: SparseMtx<_>)`- multiplies two matrices.
*	`multiplyParallel (semiring: Semiring<_>) (depth: int) (matrix1: SparseMtx<_>) (matrix2: SparseMtx<_>)` - multiplies two matrices concurrently. Number of threads = 4^depth.
*	`kroneckerProduct (semiring: Semiring<_>) (matrix1: SparseMtx<_>) (matrix2: SparseMtx<_>)` - calculates Kronecker product of two matrices.
*	`closure (semiring: Semiring<_>) (matrix: SparseMtx<_>)` - transitive closure of two matrices.

## SparseMtx

SparseMtx is a module with standard functions on matrices.

* `init (size: int) (operators: Operators<'a>) (initializer: int -> int -> 'a)` - creates new matrix with initializer given.
* `iteri (iterator: int -> int -> 'a -> unit) (matrix: SparseMtx<_>)` - applies the given func to each element of matrix. The integers passed to the function indicates the coordinates of element.
* `iter (iterator: 'a -> unit) (matrix: SparseMtx<_>)` - applies the given func to each element of matrix.
* `mapi (outerTypeOps: Operators<'b>) (mapping: int -> int -> 'a -> 'b) (matrix: SparseMtx<'a>)` - builds a new matrix whose elements are the results of applying the given function to each of the elements of the matrix. The integers passed to the function indicates the coordinates of element being transformed.
* `map (outerTypeOps: Operators<'b>) (mapping: 'a -> 'b) (matrix: SparseMtx<'a>)` - builds a new matrix whose elements are the results of applying the given function to each of the elements of the matrix.
* `toArray2D (matrix: SparseMtx<_>)` - transforms matrix to Array2D.
* `doubleSize (matrix: SparseMtx<_>)` - returns matrix of 2X size.
* `isEqual (matrix1: SparseMtx<_>) (matrix2: SparseMtx<_>)` - equality of two matrices.

## SparseArray2D

SparseArray2D is a module with operations on matrices based on Array2D.

* `getBounds (matrix: 'a [,])` - returns struct with dimensions of Array2D.
* `genIntMatrix (rows: int, cols: int) (sparsity: float)` - generates matrix with sparsity specified.
* `add (sr: Semiring<'a>) (matrix1: 'a [,]) (matrix2: 'a [,])` - adds two matrices.
* `multiply (sr: Semiring<'a>) (matrix1: 'a [,]) (matrix2: 'a [,])`- multiplies two matrices.
* `kroneckerProduct (sr: Semiring<'a>) (matrix1: 'a [,]) (matrix2: 'a [,])` - calculates Kronecker product of two matrices.
* `closure (sr: Semiring<'a>) (predicateFunc: 'a -> bool) (matrix: 'a [,])` - transitive closure of two matrices with predicate function specified.

