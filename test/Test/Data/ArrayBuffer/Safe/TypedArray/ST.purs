module Test.Data.ArrayBuffer.Safe.TypedArray.ST where

import Prelude hiding (eq)

import Control.Monad.ST (pureST)
import Data.Array as A
import Data.ArrayBuffer.Safe.TypedArray (Int8Array, Uint32Array)
import Data.ArrayBuffer.Safe.TypedArray as TA
import Data.ArrayBuffer.Safe.TypedArray.ST (newSTTypedArray, unsafeFreeze)
import Data.ArrayBuffer.Safe.TypedArray.ST as STTA
import Data.Maybe (Maybe, fromJust, isNothing)
import Data.Ord (abs)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

testSTTypedArray :: Spec (QCRunnerEffects ()) Unit
testSTTypedArray = describe "STTypedArray" $ do
  describe "newSTTypedArray" $ do
    describe "Int8Array" $ do
      it "creates an empty array of the desired size" $
        quickCheck \sz ->
          let posSz = abs sz `mod` 10000
          in  (unsafePartial $ fromJust
               $ pureST (traverse unsafeFreeze =<< newSTTypedArray posSz)
              )
              `TA.eq` (unsafePartial $ fromJust $ TA.newTypedArray posSz) :: Int8Array
      it "returns Nothing on attempts to make negatively sized arrays" $
        quickCheck \sz ->
          let posSz = - (abs sz)
          in  isNothing
                ((pureST (traverse unsafeFreeze =<< newSTTypedArray posSz))
                  :: Maybe Int8Array)
    describe "Uint32Array" $ do
      it "creates an empty array of the desired size" $
        quickCheck \sz ->
          let posSz = abs sz `mod` 10000
          in  (unsafePartial $ fromJust
               $ pureST (traverse unsafeFreeze =<< newSTTypedArray posSz)
              )
              `TA.eq` (unsafePartial $ fromJust $ TA.newTypedArray posSz) :: Uint32Array
      it "returns Nothing on attempts to make negatively sized arrays" $
        quickCheck \sz ->
          let posSz = - (abs sz)
          in  isNothing
                ((pureST (traverse unsafeFreeze =<< newSTTypedArray posSz))
                  :: Maybe Uint32Array)
  describe "fill" $ do
    describe "Int32Array" $ do
      it "fills the array with the desired value" $
        quickCheck \sz val ->
          let posSz = abs sz `mod` 10000
          in  TA.toArray ((pureST do
                stArr <- (unsafePartial $ fromJust ) <$> newSTTypedArray posSz
                unsafeFreeze =<< STTA.fill stArr val
              ) :: TA.Int32Array)
              == A.replicate posSz val
  describe "fillFrom" $ do
    describe "Int32Array" $ do
      it "fills the array with the desired value" $
        quickCheck \sz from val ->
          let posSz = abs sz `mod` 10000
              from' = abs from `mod` posSz
          in  TA.toArray ((pureST do
                stArr <- (unsafePartial $ fromJust ) <$> newSTTypedArray posSz
                unsafeFreeze =<< STTA.fillFrom stArr val from'
              ) :: TA.Int32Array)
              == A.replicate from' 0 <> A.replicate (posSz - from') val
  describe "fillFromTo" $ do
    describe "Int32Array" $ do
      it "fills the array with the desired value" $
        quickCheck \sz from to val ->
          let posSz = abs sz `mod` 10000
              from' = abs from `mod` posSz
              to' = from' + (abs to `mod` (posSz - from'))
          in  TA.toArray ((pureST do
                stArr <- (unsafePartial $ fromJust ) <$> newSTTypedArray posSz
                unsafeFreeze =<< STTA.fillFrom stArr val from'
              ) :: TA.Int32Array)
              == A.replicate from' 0
                 <> A.replicate (from' - to') val
                 <> A.replicate (posSz - from') val
