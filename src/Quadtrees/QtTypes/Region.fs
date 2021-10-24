namespace Quadtrees.QtTypes

module Region =
    open Direction

    /// Abstract region in two-dimensional space
    type IRegion<'coord> =
        abstract SizeX : 'coord
        abstract SizeY : 'coord
        abstract member Contains : 'coord * 'coord -> bool
        abstract member GetDirection : 'coord * 'coord -> Direction
        abstract member SplitPossible : unit -> bool
        abstract member Split : Direction -> IRegion<'coord>
        abstract member Copy : unit -> IRegion<'coord>
