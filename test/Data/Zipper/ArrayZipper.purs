module Test.Data.Zipper.ArrayZipper where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Data.Zipper.ArrayZipper (ArrayZipper, asArrayZipper, getFocus, modifyFocus, next, prev, pushNextRefocus, pushPrevRefocus, setFocus, shiftFocusBy, shiftFocusBy', shiftFocusFirst, shiftFocusLast, toArrayZipperAt, toArrayZipperAt', toArrayZipperFirst, toArrayZipperLast)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Array Zipper" do
  describe "Construction" do
    it "valid constructions" do
      (asArrayZipper 1) `shouldEqual` (mkZipper 0 [1])
      (getFocus <$> (toArrayZipperFirst [1])) `shouldEqual` (Just 1)
      (getFocus <$> toArrayZipperLast [0, 1]) `shouldEqual` (Just 1)
      (getFocus <$> toArrayZipperAt (-1) [0, 1, 2]) `shouldEqual` (Just 0)
      (getFocus <$> toArrayZipperAt   0  [0, 1, 2]) `shouldEqual` Just 0
      (getFocus <$> toArrayZipperAt   1  [0, 1, 2]) `shouldEqual` Just 1
      (getFocus <$> toArrayZipperAt   2  [0, 1, 2]) `shouldEqual` Just 2
      (getFocus <$> toArrayZipperAt   3  [0, 1, 2]) `shouldEqual` Just 2

      (getFocus <$> toArrayZipperAt'   0  [0, 1, 2]) `shouldEqual` Just 0
      (getFocus <$> toArrayZipperAt'   1  [0, 1, 2]) `shouldEqual` Just 1
      (getFocus <$> toArrayZipperAt'   2  [0, 1, 2]) `shouldEqual` Just 2

    it "invalid constructions" do
      toArrayZipperFirst emptyArray `shouldEqual` Nothing
      toArrayZipperLast emptyArray `shouldEqual` Nothing
      toArrayZipperAt' (-1) [0, 1, 2] `shouldEqual` Nothing
      toArrayZipperAt'    3 [0, 1, 2] `shouldEqual` Nothing

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
