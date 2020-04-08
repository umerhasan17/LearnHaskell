## Course Notes

Different types of lists: 
    * default lists (defined as `data [a] = [] | (:) a [a]`, n time append, ) 
    * tree lists (constant append, head worst case is n)
    * difference lists (constant cons, snoc, append)



## Language Features

Infix functions are defined using brackets for example `(++):: list a -> list a -> list a`. 

Remember to put the dot for function composition. Can also bracket the functions during function composition without arguments i.e. `normalize = (fromList . toList)` or `normalize = fromList . toList`.

`newtype` keyword same as `data` except it can only have 1 constructor with 1 field. This essentially makes the `newtype` of the same type as the field but with a different name. `newtype` requires a new constructor.

`type` is a synonym for a type and uses the same data constructors.

`data` needs an optional context, the type name, a variable number of type variables, a variable number of constructors, and optional deriving. 

[1, 2, 3, 4, 5]
foldr 
foldl 

## Errors

Specify ambigious functions if redefenitions occur for example when redefining `length` use `Prelude.length` for the old one and `Filename.length` or `Main.length` for the new one.

## Help

`:i function name` gives information in GHCI `:r` gives types.