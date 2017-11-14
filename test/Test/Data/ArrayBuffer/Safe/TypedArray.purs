module Test.Data.ArrayBuffer.Safe.TypedArray where

import Prelude

import Data.Array as A
import Data.ArrayBuffer.Safe.ArrayBuffer as AB
import Data.ArrayBuffer.Safe.TypedArray as TA
import Data.ArrayBuffer.Safe.TypedArray.Int8Array as I8A
import Data.Maybe (Maybe, isNothing, maybe)
import Data.Ord (abs)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

testTypedArray :: Spec (QCRunnerEffects ()) Unit
testTypedArray = describe "TypedArray" do
  describe "eq" $ do
    describe "Int8Array" do
      it "returns true when equal" $
        quickCheck \xs ->
          TA.fromArray xs `TA.eq` (TA.fromArray xs :: TA.Int8Array)
      {- might fail due to different 32-bit ints becoming same 8-bit :( 
      it "returns false when unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            not $ TA.fromArray xs `TA.eq` (TA.fromArray ys :: TA.Int8Array)
      -}
    describe "Int16Array" do
      it "returns true when equal" $
        quickCheck \xs ->
          TA.fromArray xs `TA.eq` (TA.fromArray xs :: TA.Int16Array)
      {- might fail due to different 32-bit ints becoming same 16-bit :( 
      it "returns false when unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            not $ TA.fromArray xs `TA.eq` (TA.fromArray ys :: TA.Int16Array)
      -}
    describe "Int32Array" do
      it "returns true when equal" $
        quickCheck \xs ->
          TA.fromArray xs `TA.eq` (TA.fromArray xs :: TA.Int32Array)
      it "returns false when unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            not $ TA.fromArray xs `TA.eq` (TA.fromArray ys :: TA.Int32Array)
    describe "Float64Array" do
      it "returns true when equal" $
        quickCheck \xs ->
          TA.fromArray xs `TA.eq` (TA.fromArray xs :: TA.Float64Array)
      it "returns false when unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            not $ TA.fromArray xs `TA.eq` (TA.fromArray ys :: TA.Float64Array)
  describe "notEq" $ do
    describe "Int8Array" do
      it "returns false for equal" $
        quickCheck \xs ->
          not $ TA.fromArray xs `TA.notEq` (TA.fromArray xs :: TA.Int8Array)
      {- might fail due to different 32-bit ints becoming same 8-bit :( 
      it "returns true for unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            TA.fromArray xs `TA.notEq` (TA.fromArray ys :: TA.Int8Array)
      -}
    describe "Int16Array" do
      it "returns false for equal" $
        quickCheck \xs ->
          not $ TA.fromArray xs `TA.notEq` (TA.fromArray xs :: TA.Int16Array)
      {- might fail due to different 32-bit ints becoming same 16-bit :( 
      it "returns true for unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            TA.fromArray xs `TA.notEq` (TA.fromArray ys :: TA.Int16Array)
      -}
    describe "Int32Array" do
      it "returns false for equal" $
        quickCheck \xs ->
          not $ TA.fromArray xs `TA.notEq` (TA.fromArray xs :: TA.Int32Array)
      it "returns true for unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            TA.fromArray xs `TA.notEq` (TA.fromArray ys :: TA.Int32Array)
    describe "Float64Array" do
      it "returns false for equal" $
        quickCheck \xs ->
          not $ TA.fromArray xs `TA.notEq` (TA.fromArray xs :: TA.Float64Array)
      it "returns true for unequal" $
        quickCheck \xs ->
          arbitrary `suchThat` notEq xs <#> \ys ->
            TA.fromArray xs `TA.notEq` (TA.fromArray ys :: TA.Float64Array)
  describe "slice" $ do
    describe "Int32Array" do
      it "slices like Array.slice" $
        quickCheck \xs begin end ->
          TA.fromArray (A.slice begin end xs) :: TA.Int32Array
          `TA.eq` TA.slice (TA.fromArray xs) begin end
      it "slices like Array.slice (begin in array, end = length)" $
        quickCheck \xs begin ->
          let len = A.length xs
              begin' = abs (begin `mod` len)
          in TA.fromArray (A.slice begin' len xs) :: TA.Int32Array
             `TA.eq` TA.slice (TA.fromArray xs) begin' len
      it "slices like Array.slice (begin = 0, end in array)" $
        quickCheck \xs end ->
          let len = A.length xs
              end' = abs (end `mod` len)
          in TA.fromArray (A.slice 0 end' xs) :: TA.Int32Array
             `TA.eq` TA.slice (TA.fromArray xs) 0 end'
  describe "take" $ do
    describe "Int32Array" do
      it "takes like Array.take (for positive indexes)" $
        quickCheck \xs length ->
          let length' = abs length
          in  TA.fromArray (A.take length' xs) :: TA.Int32Array
              `TA.eq` TA.take (TA.fromArray xs) length'
      it "takes like Array.take (length in array)" $
        quickCheck \xs length ->
          let alen = A.length xs
              length' = abs (length `mod` alen)
          in  TA.fromArray (A.take length' xs) :: TA.Int32Array
              `TA.eq` TA.take (TA.fromArray xs) length'
  describe "drop" $ do
    describe "Int32Array" do
      it "drops like Array.drop (for positive indexes)" $
        quickCheck \xs length ->
          let length' = abs length
          in  TA.fromArray (A.drop length' xs) :: TA.Int32Array
              `TA.eq` TA.drop (TA.fromArray xs) length'
      it "drops like Array.drop (length in array)" $
        quickCheck \xs length ->
          let alen = A.length xs
              length' = abs (length `mod` alen)
          in  TA.fromArray (A.drop length' xs) :: TA.Int32Array
              `TA.eq` TA.drop (TA.fromArray xs) length'
  describe "fromArrayBufferWithOffset" $ do
    describe "Int8Array" do
      it "correctly captures the desired part of the ArrayBuffer" $
        quickCheck \xs byteOffset ->
          let i8a = TA.fromArray xs :: TA.Int8Array
              ab = TA.buffer i8a
              byteOffset' = abs $ byteOffset `mod` AB.byteLength ab
              abSliced = TA.buffer $ TA.drop i8a byteOffset'
              maybeOffsetInt8Array = TA.fromArrayBufferWithOffset ab byteOffset'
              int8ArrayFromSlicedAB = I8A.fromArrayBuffer abSliced
          in  maybe false (_ `TA.eq` int8ArrayFromSlicedAB) maybeOffsetInt8Array
      it "returns Nothing with offset greater than ArrayBuffer length" $
        quickCheck \xs byteOffset ->
          let i8a = TA.fromArray xs :: TA.Int8Array
              ab = TA.buffer i8a
              byteOffset' = AB.byteLength ab + abs byteOffset
              maybeOffsetInt8Array :: Maybe TA.Int8Array
              maybeOffsetInt8Array = TA.fromArrayBufferWithOffset ab byteOffset'
          in  isNothing maybeOffsetInt8Array
      it "returns Nothing with negative offset" $
        quickCheck \xs byteOffset ->
          let i8a = TA.fromArray xs :: TA.Int8Array
              ab = TA.buffer i8a
              byteOffset' = -1 - abs byteOffset
              maybeOffsetInt8Array :: Maybe TA.Int8Array
              maybeOffsetInt8Array = TA.fromArrayBufferWithOffset ab byteOffset'
          in  isNothing maybeOffsetInt8Array
  describe "fromArrayBufferWithOffsetAndLength" $ do
    describe "Int8Array" do
      it "correctly captures the desired part of the ArrayBuffer" $
        quickCheck \xs byteOffset length ->
          let i8a = TA.fromArray xs :: TA.Int8Array
              ab = TA.buffer i8a
              byteOffset' = abs $ byteOffset `mod` AB.byteLength ab
              length' = abs (length `mod` (AB.byteLength ab - byteOffset'))
              abSliced = TA.buffer $ TA.slice i8a byteOffset' (byteOffset' + length')
              maybeOffsetInt8Array =
                TA.fromArrayBufferWithOffsetAndLength ab byteOffset' length'
              int8ArrayFromSlicedAB = I8A.fromArrayBuffer abSliced
          in  maybe false (_ `TA.eq` int8ArrayFromSlicedAB) maybeOffsetInt8Array
      it "returns Nothing with offset greater than ArrayBuffer length" $
        quickCheck \xs byteOffset ->
          let i8a = TA.fromArray xs :: TA.Int8Array
              ab = TA.buffer i8a
              byteOffset' = AB.byteLength ab + abs byteOffset
              maybeOffsetInt8Array :: Maybe TA.Int8Array
              maybeOffsetInt8Array =
                TA.fromArrayBufferWithOffsetAndLength ab byteOffset' 0
          in  isNothing maybeOffsetInt8Array
      it "returns Nothing with negative offset" $
        quickCheck \xs byteOffset ->
          let i8a = TA.fromArray xs :: TA.Int8Array
              ab = TA.buffer i8a
              byteOffset' = -1 - abs byteOffset
              maybeOffsetInt8Array :: Maybe TA.Int8Array
              maybeOffsetInt8Array =
                TA.fromArrayBufferWithOffsetAndLength ab byteOffset' 0
          in  isNothing maybeOffsetInt8Array
      it "returns Nothing with length overlapping ArrayBuffer length" $
        quickCheck \xs byteOffset length ->
          let i8a = TA.fromArray xs :: TA.Int8Array
              ab = TA.buffer i8a
              byteOffset' = abs $ byteOffset `mod` AB.byteLength ab
              length' = abs (AB.byteLength ab - byteOffset') + abs length
              maybeOffsetInt8Array :: Maybe TA.Int8Array
              maybeOffsetInt8Array =
                TA.fromArrayBufferWithOffsetAndLength ab byteOffset' length'
          in  isNothing maybeOffsetInt8Array
      it "returns Nothing with negative length" $
        quickCheck \xs byteOffset length ->
          let i8a = TA.fromArray xs :: TA.Int8Array
              ab = TA.buffer i8a
              byteOffset' = abs $ byteOffset `mod` AB.byteLength ab
              length' = 1 - abs length
              maybeOffsetInt8Array :: Maybe TA.Int8Array
              maybeOffsetInt8Array =
                TA.fromArrayBufferWithOffsetAndLength ab byteOffset' length'
          in  isNothing maybeOffsetInt8Array
