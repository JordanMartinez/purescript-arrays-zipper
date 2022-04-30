module Data.Zipper.ArrayZipper
  ( ArrayZipper -- constructor not exported intentionally
  , asArrayZipper
  , toArrayZipperFirst
  , toArrayZipperLast
  , toArrayZipperAt
  , toArrayZipperAt'

  , exposeArray
  , exposeMaxIndex
  , exposeFocusIndex

  , hasPrev
  , hasNext

  , prev
  , next

  , shiftFocusBy
  , shiftFocusBy'
  , shiftFocusByFind
  , shiftFocusByFind'
  , shiftFocusTo
  , shiftFocusTo'
  , shiftFocusFirst
  , shiftFocusLast

  , getFocus
  , setFocus
  , modifyFocus

  , pushPrev
  , pushNext
  , pushPrevRefocus
  , pushNextRefocus
  )
  where

import Prelude

import Control.Comonad (class Comonad)
import Control.Extend (class Extend)
import Control.Monad.Gen (chooseInt)
import Data.Array (findIndex, length, mapWithIndex, unsafeIndex)
import Data.Array.NonEmpty as NEA
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)

-- | An immutable Zipper for an Array.
-- |
-- | This Zipper works well in read-heavy code
-- | but might not work well in write-heavy code
-- |
-- | Modifications to the focused element are `O(n)` due to creating
-- | a new immutable array with the change rather than mutating the
-- | underlying array.
-- |
-- | Navigating to a new focus element is `O(1)` regardless of how far
-- | away from the current focus that element is. This
-- | is in contrast to a `List`-based zipper where modifications
-- | are `O(1)` and navigation is `O(n)`.
-- |
-- | ```
-- | [0, 1, 2, 3, 4, 5] <-- underlying array
-- |          ^      ^
-- |          |      -- maxIndex
-- |          -- focusIndex
-- | ```
newtype ArrayZipper a = ArrayZipper { array :: Array a, focusIndex :: Int, maxIndex :: Int }

derive instance eqArrayZipper :: Eq a => Eq (ArrayZipper a)
derive instance ordArrayZipper :: Ord a => Ord (ArrayZipper a)
derive instance functorArrayZipper :: Functor ArrayZipper

instance showArrayZipper :: Show a => Show (ArrayZipper a) where
  show (ArrayZipper r) = "ArrayZipper(" <> show r <> ")"

instance functorWithIndexArrayZipper :: FunctorWithIndex Int ArrayZipper where
  mapWithIndex f (ArrayZipper r) = ArrayZipper r { array = mapWithIndex f r.array }

instance foldableArrayZipper :: Foldable ArrayZipper where
  foldl f init (ArrayZipper r) = foldl f init r.array

  foldr f last (ArrayZipper r) = foldr f last r.array

  foldMap = foldMapDefaultL

instance foldableWithIndexArrayZipper :: FoldableWithIndex Int ArrayZipper where
  foldlWithIndex f init (ArrayZipper r) = foldlWithIndex f init r.array

  foldrWithIndex f last (ArrayZipper r) = foldrWithIndex f last r.array

  foldMapWithIndex f (ArrayZipper r) = foldMapWithIndex f r.array

instance traversableArrayZipper :: Traversable ArrayZipper where
  traverse f (ArrayZipper r) = ado
    ar <- traverse f r.array
    in (ArrayZipper r { array = ar })

  sequence = sequenceDefault

instance traversableWithIndexArrayZipper :: TraversableWithIndex Int ArrayZipper where
  traverseWithIndex f (ArrayZipper r) = ado
    ar <- traverseWithIndex f r.array
    in (ArrayZipper r { array = ar })

-- | Given a function, `f`, passes in `n`-many versions of this zipper to that
-- | function where `n` corresponds to the number of elements within the array
-- | and each version of the zipper will focus the element at the `n`th index.
instance extendArrayZipper :: Extend ArrayZipper where
  extend :: forall b a. (ArrayZipper a -> b) -> ArrayZipper a -> ArrayZipper b
  extend f (ArrayZipper rec) =
    let allFoci idx _ = f (ArrayZipper rec { focusIndex = idx })
    in ArrayZipper (rec { array = mapWithIndex allFoci rec.array})

-- | Gets the focused element (i.e. same as `getFocus`).
instance comonadArrayZipper :: Comonad ArrayZipper where
  extract :: forall a. ArrayZipper a -> a
  extract = getFocus

-- Test-related items
instance arbitraryArrayZipper :: Arbitrary a => Arbitrary (ArrayZipper a) where
  arbitrary = do
    array <- NEA.toArray <$> arbitrary
    let maxIndex = length array - 1
    focusIndex <- chooseInt 0 maxIndex
    pure $ ArrayZipper { array, focusIndex, maxIndex }

instance coarbitraryArrayZipper :: Coarbitrary a => Coarbitrary (ArrayZipper a) where
  coarbitrary (ArrayZipper r) =
    coarbitrary r.array >>>
    coarbitrary r.maxIndex >>>
    coarbitrary r.focusIndex

-- | Creates an Array Zipper from a single element. This will be stored
-- | internally as a 1-element array. To further build upon this array,
-- | see `push*` functions.
asArrayZipper :: forall a. a -> ArrayZipper a
asArrayZipper a = ArrayZipper { array: [a], focusIndex: 0, maxIndex: 0 }

-- | Returns `Nothing` if the array is empty. Otherwise, returns an ArrayZipper
-- | with the first element as the focus.
toArrayZipperFirst :: forall a. Array a -> Maybe (ArrayZipper a)
toArrayZipperFirst = case _ of
  [] -> Nothing
  array -> Just (ArrayZipper { array, focusIndex: 0, maxIndex: length array - 1 })

-- | Returns `Nothing` if the array is empty. Otherwise, returns an ArrayZipper
-- | with the last element as the focus.
toArrayZipperLast :: forall a. Array a -> Maybe (ArrayZipper a)
toArrayZipperLast = case _ of
  [] -> Nothing
  array ->
    let maxIndex = length array - 1
    in Just (ArrayZipper { array, focusIndex: maxIndex, maxIndex })

-- | Returns `Nothing` if the array is empty. Otherwise, returns an ArrayZipper
-- | with the element at the given index as the focus. The given index
-- | will be clamped within the array's bounds to ensure it always refers
-- | to a valid element in the array. To return `Nothing` on an invalid index,
-- | see `toArrayZipperAt'`.
toArrayZipperAt :: forall a. Int -> Array a -> Maybe (ArrayZipper a)
toArrayZipperAt startingFocusIndex = case _ of
  [] -> Nothing
  array ->
    let
      maxIndex = length array - 1
      focusIndex = clamp 0 maxIndex startingFocusIndex
    in Just (ArrayZipper { array, focusIndex, maxIndex })

-- | Returns `Nothing` if the array is empty or if the given index is
-- | outside the bounds of the array. Otherwise, returns an ArrayZipper
-- | with the element at the given index as the focus. To return `Just zipper`
-- | by clamping an invalid index, so that it is within the array, see
-- | `toArrayZipperAt`.
toArrayZipperAt' :: forall a. Int -> Array a -> Maybe (ArrayZipper a)
toArrayZipperAt' focusIndex = case _ of
  [] -> Nothing
  array ->
    let
      maxIndex = length array - 1
    in
      if 0 <= focusIndex && focusIndex <= maxIndex
        then Just (ArrayZipper { array, focusIndex, maxIndex })
        else Nothing

-- | Exposes the underlying array. **Note:** any mutations to this array via
-- | `unsafeThaw` will invalidate the constraints guaranteed by `ArrayZipper`.
exposeArray :: forall a. ArrayZipper a -> Array a
exposeArray (ArrayZipper r) = r.array

-- | Exposes the index of the last element
exposeMaxIndex :: forall a. ArrayZipper a -> Int
exposeMaxIndex (ArrayZipper r) = r.maxIndex

-- | Exposes the index of the focused element
exposeFocusIndex :: forall a. ArrayZipper a -> Int
exposeFocusIndex (ArrayZipper r) = r.focusIndex

-- | Returns `true` if `prev` will return a `Just`
hasPrev :: forall a. ArrayZipper a -> Boolean
hasPrev (ArrayZipper r) = r.focusIndex > 0

-- | Returns `true` if `prev` will return a `Just`
hasNext :: forall a. ArrayZipper a -> Boolean
hasNext (ArrayZipper r) = r.focusIndex < r.maxIndex

-- | Returns `Nothing` if the focus element is the first element in the array.
-- | Otherwise, returns `Just` where the new focus element is the previous
-- | element.
prev :: forall a. ArrayZipper a -> Maybe (ArrayZipper a)
prev (ArrayZipper r)
  | r.focusIndex - 1 >= 0 = Just (ArrayZipper r { focusIndex = r.focusIndex - 1 })
  | otherwise = Nothing

-- | Returns `Nothing` if the focus element is the last element in the array.
-- | Otherwise, returns `Just` where the new focus element is the next
-- | element.
next :: forall a. ArrayZipper a -> Maybe (ArrayZipper a)
next (ArrayZipper r)
  | r.focusIndex + 1 <= r.maxIndex = Just (ArrayZipper r { focusIndex = r.focusIndex + 1 })
  | otherwise = Nothing

-- | Use a function to focus a different element in the array by using the
-- | zipper's current focus index. If the resulting index is outside the bounds
-- | of the array, the index will refer to the first or last element, whichever
-- | is closer to the output of the function. To prevent clamping
-- | and return Nothing if the output of the function is an invalid index,
-- | see `shiftFocusBy'`.
shiftFocusBy :: forall a. (Int -> Int) -> ArrayZipper a -> ArrayZipper a
shiftFocusBy f (ArrayZipper r) =
  let updatedFocusIndex = clamp 0 r.maxIndex (f r.focusIndex)
  in (ArrayZipper r { focusIndex = updatedFocusIndex })

-- | Use a function to focus a different element in the array by using the
-- | zipper's current focus index. If the resulting index is outside the bounds
-- | of the array, `Nothing` is returned. If it's a valid index, `Just zipper`
-- | is returned.
shiftFocusBy' :: forall a. (Int -> Int) -> ArrayZipper a -> Maybe (ArrayZipper a)
shiftFocusBy' f (ArrayZipper r) =
  let updatedFocusIndex = f r.focusIndex
  in if 0 <= updatedFocusIndex && updatedFocusIndex <= r.maxIndex
      then Just (ArrayZipper r { focusIndex = updatedFocusIndex })
      else Nothing

-- | Use a function to find and focus the first matching element in the array.
-- | If no element matches, the zipper is returned unchanged.
shiftFocusByFind :: forall a. (a -> Boolean) -> ArrayZipper a -> ArrayZipper a
shiftFocusByFind f zipper = fromMaybe zipper $ shiftFocusByFind' f zipper

-- | Use a function to find and the first matching element in the array.
-- | If no element matches, `Nothing` is returned.
-- | If an element matches, `Just zipper` is returned.
shiftFocusByFind' :: forall a. (a -> Boolean) -> ArrayZipper a -> Maybe (ArrayZipper a)
shiftFocusByFind' f (ArrayZipper r) = do
  index <- findIndex f r.array
  pure $ ArrayZipper $ r { focusIndex = index }

-- | Find and focus the first equal element in the array.
-- | If no element is equal, the zipper is returned unchanged.
shiftFocusTo :: forall a. Eq a => a -> ArrayZipper a -> ArrayZipper a
shiftFocusTo a zipper = shiftFocusByFind ((==) a) zipper

-- | Find and focus the first equal element in the array.
-- | If no element is equal, `Nothing` is returned.
-- | If an element is equal, `Just zipper` is returned.
shiftFocusTo' :: forall a. Eq a => a -> ArrayZipper a -> Maybe (ArrayZipper a)
shiftFocusTo' a zipper = shiftFocusByFind' ((==) a) zipper

-- | Changes the focus element to the first element in the array.
shiftFocusFirst :: forall a. ArrayZipper a -> ArrayZipper a
shiftFocusFirst (ArrayZipper r) = ArrayZipper r { focusIndex = 0 }

-- | Changes the focus element to the last element in the array.
shiftFocusLast :: forall a. ArrayZipper a -> ArrayZipper a
shiftFocusLast (ArrayZipper r) = ArrayZipper r { focusIndex = r.maxIndex }

-- Note: these FFI functions exist because `purescript-arrays` does not export
-- them.
foreign import unsafeInsertAt :: forall a. Int -> a -> Array a -> Array a
foreign import unsafeSetAt :: forall a. Int -> a -> Array a -> Array a
foreign import unsafeModifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a

-- | Returns the focus element. `O(1)`
getFocus :: forall a. ArrayZipper a -> a
getFocus (ArrayZipper r) = unsafePartial (unsafeIndex r.array r.focusIndex)

-- | Sets the focus element. `O(n)`
setFocus :: forall a. a -> ArrayZipper a -> ArrayZipper a
setFocus a (ArrayZipper r) = ArrayZipper (r { array = unsafeSetAt r.focusIndex a r.array })

-- | Uses a function to update the focus element. `O(n)`
modifyFocus :: forall a. (a -> a) -> ArrayZipper a -> ArrayZipper a
modifyFocus f (ArrayZipper r) = ArrayZipper (r { array = unsafeModifyAt r.focusIndex f r.array })

-- | Inserts an element in front of / to the left of the focus element. `O(n)`
pushPrev :: forall a. a -> ArrayZipper a -> ArrayZipper a
pushPrev a (ArrayZipper r) =
  ArrayZipper { focusIndex: r.focusIndex + 1
              , maxIndex: r.maxIndex + 1
              , array: unsafeInsertAt r.focusIndex a r.array
              }

-- | Inserts an element behind / to the right of the focus element. `O(n)`
pushNext :: forall a. a -> ArrayZipper a -> ArrayZipper a
pushNext a (ArrayZipper r) =
  ArrayZipper r { maxIndex = r.maxIndex + 1
                , array = unsafeInsertAt (r.focusIndex + 1) a r.array
                }

-- | Inserts an element in front of / to the left of the focus element
-- | and sets this new element as the focus element. `O(n)`
pushPrevRefocus :: forall a. a -> ArrayZipper a -> ArrayZipper a
pushPrevRefocus a (ArrayZipper r) =
    ArrayZipper r { maxIndex = r.maxIndex + 1
                  , array = unsafeInsertAt r.focusIndex a r.array
                  }

-- | Inserts an element behind / to the right of the focus element
-- | and sets this new element as the focus element. `O(n)`
pushNextRefocus :: forall a. a -> ArrayZipper a -> ArrayZipper a
pushNextRefocus a (ArrayZipper r) =
    ArrayZipper r { focusIndex = r.focusIndex + 1
                  , maxIndex = r.maxIndex + 1
                  , array = unsafeInsertAt (r.focusIndex + 1) a r.array
                  }
