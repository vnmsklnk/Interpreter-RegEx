# Quadtrees

MyList is a non-empty list library which is used in a BigInt module.

## Type

`MyList` is a discriminated union with `One of 't` or `Cons of 't * MyList<t>`. That makes `MyList` a non-empty list 

## Functions

* `head (_arg1:MyList<'t>)` - returns a head of a list
* `tail (_arg1:MyList<'t>)` - reurns a tail of a list
* `fold (folder:'a -> 'b -> 'a) (acc:'a) (l:MyList<'b>)` - changes a sign of a number
* `len (l:MyList<'t>)` - returns a length of a list
* `concat (l1:MyList<'t>) (l2:MyList<'t>)` - deletes all zeroes from the begining of a list
* `map (mapping:'a -> 'b) (l:MyList<'a>)` - returns a list to whose elements the given function has been applied 
* `iter (action:'a -> unit) (l:MyList<'a'>)` - applies a given function to each element of a list
* `bubbleSort (l:MyList<'t>)` - returns a sorted list
* `listToMyList (l:List<'t>)` - converts `List` to `MyList`
* `myListToList (l:MyList<'t>)` - converts `MyList` to `List`
* `reverse (x:BigInt)` - returns a list with elements in a reversed order
* `map2 (mapping:'a -> 'b) (x:MyList<'a>) (y:MyList<'a>)` -  returns two list to whose elements the given function has been applied 
* `intToMyList (i:int)` - converts `int` to `MyList`