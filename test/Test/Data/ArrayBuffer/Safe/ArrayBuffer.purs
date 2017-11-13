module Test.Data.ArrayBuffer.Safe.ArrayBuffer where

import Prelude

import Data.ArrayBuffer.Safe.ArrayBuffer as AB
import Data.ArrayBuffer.Safe.TypedArray as TA
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

testArrayBuffer :: Spec (QCRunnerEffects ()) Unit
testArrayBuffer = describe "ArrayBuffer" do
  describe "eq" $ do
    it "returns true with equal ArrayBuffers" $
      quickCheck \xs ->
        TA.buffer (TA.fromArray xs :: TA.Int32Array)
        `AB.eq` TA.buffer (TA.fromArray xs :: TA.Int32Array)
    it "returns false when unequal" $
      quickCheck \xs ->
        arbitrary `suchThat` notEq xs <#> \ys ->
          not $ TA.buffer (TA.fromArray xs :: TA.Int32Array)
                `AB.eq` TA.buffer (TA.fromArray ys :: TA.Int32Array)
  describe "notEq" $ do
    it "returns false for equal" $
      quickCheck \xs ->
        not $ TA.buffer (TA.fromArray xs :: TA.Int32Array)
              `AB.notEq` TA.buffer (TA.fromArray xs :: TA.Int32Array)
    it "returns true for unequal" $
      quickCheck \xs ->
        arbitrary `suchThat` notEq xs <#> \ys ->
          TA.buffer (TA.fromArray xs :: TA.Int32Array)
          `AB.notEq` TA.buffer (TA.fromArray ys :: TA.Int32Array)
