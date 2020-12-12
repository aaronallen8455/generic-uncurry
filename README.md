# Generic Uncurry

This library provides `guncurry` which is a function that can be used to apply
functions of n-arity to product types of the respective arguments.

Works with tuples of up to 7 elements, as that is where the `Generic` instances
in `base` run out. You can also use it with any other product types that
implement `Generic`, which is useful if you want to apply the same function to
different product types that have the same corresponding field types.

```haskell
>>> guncurry (&&) (True, False)

False

>>> guncurry foldr ((+), 0 :: Int, [1,2,3,4,5])

15

data MyProduct =
  MyProduct
    { field1 :: Int
    , field2 :: Int
    } deriving Generic

>>> guncurry (*) $ MyProduct 2 3

6
```
