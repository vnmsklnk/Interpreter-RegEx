namespace Quadtrees.QuadtreeTypes

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type RegionParams =
    { Cnt: float * float
      SizeX: float
      SizeY: float }

/// Abstract region in two-dimensional space
type Region =
    abstract SizeX : float
    abstract SizeY : float
    abstract Center : float * float
    abstract member Contains : float * float -> bool
    abstract member GetDirection : float * float -> Direction
    abstract member SplitPossible : unit -> bool
    abstract member Split : Direction -> Region
    abstract member Copy : unit -> Region
    abstract member Parameters : unit -> RegionParams