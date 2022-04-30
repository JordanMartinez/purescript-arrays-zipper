module Test.Data.Zipper.ArrayZipper where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Data.Zipper.ArrayZipper (ArrayZipper, asArrayZipper, getFocus, modifyFocus, next, prev, pushNextRefocus, pushPrevRefocus, setFocus, shiftFocusBy, shiftFocusBy', shiftFocusByFind, shiftFocusByFind', shiftFocusTo, shiftFocusTo', shiftFocusFirst, shiftFocusLast, toArrayZipperAt, toArrayZipperAt', toArrayZipperFirst, toArrayZipperLast)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Laws as Laws
import Test.QuickCheck.Laws.Control (checkComonad, checkExtend)
import Test.QuickCheck.Laws.Data (checkEq, checkFoldable, checkFoldableFunctor, checkFunctor, checkFunctorWithIndex, checkOrd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "Array Zipper" do
  describe "Construction" do
    it "valid constructions" do
      (asArrayZipper 1) `shouldEqual` (mkZipper 0 [1])
      (getFocus <$> toArrayZipperFirst [1]) `shouldEqual` (Just 1)
      (getFocus <$> toArrayZipperLast [0, 1]) `shouldEqual` (Just 1)
      (getFocus <$> toArrayZipperAt (-1) [0, 1, 2]) `shouldEqual` (Just 0)
      (getFocus <$> toArrayZipperAt   0  [0, 1, 2]) `shouldEqual` Just 0
      (getFocus <$> toArrayZipperAt   1  [0, 1, 2]) `shouldEqual` Just 1
      (getFocus <$> toArrayZipperAt   2  [0, 1, 2]) `shouldEqual` Just 2
      (getFocus <$> toArrayZipperAt   3  [0, 1, 2]) `shouldEqual` Just 2

      (getFocus <$> toArrayZipperAt'  0  [0, 1, 2]) `shouldEqual` Just 0
      (getFocus <$> toArrayZipperAt'  1  [0, 1, 2]) `shouldEqual` Just 1
      (getFocus <$> toArrayZipperAt'  2  [0, 1, 2]) `shouldEqual` Just 2

    it "invalid constructions" do
      toArrayZipperFirst emptyArray `shouldEqual` Nothing
      toArrayZipperLast emptyArray `shouldEqual` Nothing
      toArrayZipperAt' (-1) [0, 1, 2] `shouldEqual` Nothing
      toArrayZipperAt'   3  [0, 1, 2] `shouldEqual` Nothing

  describe "focus relocation" do
    it "prev" do
      prev i0 `shouldEqual` Nothing
      prev i2 `shouldEqual` Just (mkZipper 1 a4)
      prev i4 `shouldEqual` Just (mkZipper 3 a4)
    it "next" do
      next i0 `shouldEqual` Just (mkZipper 1 a4)
      next i2 `shouldEqual` Just (mkZipper 3 a4)
      next i4 `shouldEqual` Nothing
    it "shift focus by" do
      shiftFocusBy (_ - 1) i0 `shouldEqual` mkZipper 0 a4
      shiftFocusBy (_ - 1) i2 `shouldEqual` mkZipper 1 a4
      shiftFocusBy (_ - 1) i4 `shouldEqual` mkZipper 3 a4

      shiftFocusBy (_ + 1) i0 `shouldEqual` mkZipper 1 a4
      shiftFocusBy (_ + 1) i2 `shouldEqual` mkZipper 3 a4
      shiftFocusBy (_ + 1) i4 `shouldEqual` mkZipper 4 a4
    it "shift focus by'" do
      shiftFocusBy' (_ - 1) i0 `shouldEqual` Nothing
      shiftFocusBy' (_ - 1) i2 `shouldEqual` Just (mkZipper 1 a4)
      shiftFocusBy' (_ - 1) i4 `shouldEqual` Just (mkZipper 3 a4)

      shiftFocusBy' (_ + 1) i0 `shouldEqual` Just (mkZipper 1 a4)
      shiftFocusBy' (_ + 1) i2 `shouldEqual` Just (mkZipper 3 a4)
      shiftFocusBy' (_ + 1) i4 `shouldEqual` Nothing
    it "shift focus by find" do
      let da = [0, 1, 1, 2]
      let da0 = mkZipper 0 da
      let da1 = mkZipper 1 da
      let da2 = mkZipper 2 da
      shiftFocusByFind ((==) 0) i0 `shouldEqual` i0
      shiftFocusByFind ((==) 0) i2 `shouldEqual` i0
      shiftFocusByFind (_ > 1) i0 `shouldEqual` i2
      shiftFocusByFind (_ > 1) i2 `shouldEqual` i2
      shiftFocusByFind (_ > 1) i4 `shouldEqual` i2
      shiftFocusByFind (_ > 10) i0 `shouldEqual` i0
      shiftFocusByFind (_ > 10) i2 `shouldEqual` i2
      shiftFocusByFind ((==) 1) da0 `shouldEqual` da1
      shiftFocusByFind (_ > 0) da0 `shouldEqual` da1
      shiftFocusByFind ((==) 1) da1 `shouldEqual` da1
      shiftFocusByFind (_ > 0) da1 `shouldEqual` da1
      shiftFocusByFind ((==) 1) da2 `shouldEqual` da1
      shiftFocusByFind (_ > 0) da2 `shouldEqual` da1
      shiftFocusByFind (_ > 10) da1 `shouldEqual` da1
      shiftFocusByFind (_ > 10) da2 `shouldEqual` da2
    it "shift focus by find'" do
      let da = [0, 1, 1, 2]
      let da0 = mkZipper 0 da
      let da1 = mkZipper 1 da
      let da2 = mkZipper 2 da
      shiftFocusByFind' ((==) 0) i0 `shouldEqual` Just i0
      shiftFocusByFind' ((==) 0) i2 `shouldEqual` Just i0
      shiftFocusByFind' (_ > 1) i0 `shouldEqual` Just i2
      shiftFocusByFind' (_ > 1) i2 `shouldEqual` Just i2
      shiftFocusByFind' (_ > 1) i4 `shouldEqual` Just i2
      shiftFocusByFind' (_ > 10) i0 `shouldEqual` Nothing
      shiftFocusByFind' (_ > 10) i2 `shouldEqual` Nothing
      shiftFocusByFind' ((==) 1) da0 `shouldEqual` Just da1
      shiftFocusByFind' (_ > 0) da0 `shouldEqual` Just da1
      shiftFocusByFind' ((==) 1) da1 `shouldEqual` Just da1
      shiftFocusByFind' (_ > 0) da1 `shouldEqual` Just da1
      shiftFocusByFind' ((==) 1) da2 `shouldEqual` Just da1
      shiftFocusByFind' (_ > 0) da2 `shouldEqual` Just da1
      shiftFocusByFind' (_ > 10) da1 `shouldEqual` Nothing
      shiftFocusByFind' (_ > 10) da2 `shouldEqual` Nothing
    it "shift focus to" do
      shiftFocusTo 0 i0 `shouldEqual` i0
      shiftFocusTo 0 i2 `shouldEqual` i0
      shiftFocusTo 0 i4 `shouldEqual` i0
      shiftFocusTo 2 i0 `shouldEqual` i2
      shiftFocusTo 4 i0 `shouldEqual` i4
      shiftFocusTo 5 i2 `shouldEqual` i2
      shiftFocusTo (-1) i2 `shouldEqual` i2
    it "shift focus to'" do
      shiftFocusTo' 0 i0 `shouldEqual` Just i0
      shiftFocusTo' 0 i2 `shouldEqual` Just i0
      shiftFocusTo' 0 i4 `shouldEqual` Just i0
      shiftFocusTo' 2 i0 `shouldEqual` Just i2
      shiftFocusTo' 4 i0 `shouldEqual` Just i4
      shiftFocusTo' 5 i2 `shouldEqual` Nothing
      shiftFocusTo' (-1) i2 `shouldEqual` Nothing
    it "shift focus first" do
      shiftFocusFirst i0 `shouldEqual` mkZipper 0 a4
      shiftFocusFirst i2 `shouldEqual` mkZipper 0 a4
      shiftFocusFirst i4 `shouldEqual` mkZipper 0 a4
    it "shift focus last" do
      shiftFocusLast i0 `shouldEqual` mkZipper 4 a4
      shiftFocusLast i2 `shouldEqual` mkZipper 4 a4
      shiftFocusLast i4 `shouldEqual` mkZipper 4 a4

  describe "modify the zipper" do
    it "overwriting the focused element" do
      setFocus 10 i0 `shouldEqual` mkZipper 0 [10, 1, 2, 3, 4]
      setFocus 10 i2 `shouldEqual` mkZipper 2 [0, 1, 10, 3, 4]
      setFocus 10 i4 `shouldEqual` mkZipper 4 [0, 1, 2, 3, 10]
    it "modifying the focused element" do
      modifyFocus (_ + 10) i0 `shouldEqual` mkZipper 0 [10, 1, 2, 3, 4]
      modifyFocus (_ + 10) i2 `shouldEqual` mkZipper 2 [0, 1, 12, 3, 4]
      modifyFocus (_ + 10) i4 `shouldEqual` mkZipper 4 [0, 1, 2, 3, 14]
    describe "inserting and refocusing" do
      it "prev with refocus" do
        pushPrevRefocus 10 i0 `shouldEqual` mkZipper 0 [10, 0, 1, 2, 3, 4]
        pushPrevRefocus 10 i4 `shouldEqual` mkZipper 4 [0, 1, 2, 3, 10, 4]
      it "next with refocus" do
        pushNextRefocus 10 i0 `shouldEqual` mkZipper 1 [0, 10, 1, 2, 3, 4]
        pushNextRefocus 10 i4 `shouldEqual` mkZipper 5 [0, 1, 2, 3, 4, 10]

  describe "Laws" do
    it "Eq" do
      liftEffect $ checkEq  proxy1
    it "Ord" do
      liftEffect $ checkOrd proxy1

    it "Functor" do
      liftEffect $ checkFunctor proxy2
    it "FunctorWithIndex" do
      liftEffect $ checkFunctorWithIndex proxy2
    it "Foldable via foldl/foldr" do
      liftEffect $ checkFoldable proxy2
    it "Foldable via foldMap" do
      liftEffect $ checkFoldableFunctor proxy2

    it "Extend" do
      liftEffect $ checkExtend proxy2
    it "Comonad" do
      liftEffect $ checkComonad proxy2

  where
    emptyArray :: Array Int
    emptyArray = []

    mkZipper :: forall a. Int -> Array a -> ArrayZipper a
    mkZipper i array = unsafePartial $ fromJust $ toArrayZipperAt i array

    a4 :: Array Int
    a4 = [0, 1, 2, 3, 4]

    i0 :: ArrayZipper Int
    i0 = unsafePartial $ fromJust $ toArrayZipperAt 0 a4

    i2 :: ArrayZipper Int
    i2 = unsafePartial $ fromJust $ toArrayZipperAt 2 a4

    i4 :: ArrayZipper Int
    i4 = unsafePartial $ fromJust $ toArrayZipperAt 4 a4

    proxy1 = Proxy :: Proxy (ArrayZipper Laws.A)

    proxy2 = Proxy :: Proxy ArrayZipper
