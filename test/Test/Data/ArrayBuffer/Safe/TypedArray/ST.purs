module Test.Data.ArrayBuffer.Safe.TypedArray.ST where

import Prelude hiding (eq)

import Control.Monad.ST (pureST)
import Data.ArrayBuffer.Safe.TypedArray (Int8Array, eq, newTypedArray)
import Data.ArrayBuffer.Safe.TypedArray.ST (newSTTypedArray, unsafeFreeze)
import Data.Maybe (fromJust)
import Data.Ord (abs)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

testSTTypedArray :: Spec (QCRunnerEffects ()) Unit
testSTTypedArray = describe "newSTTypedArray" $ do
  describe "Int8Array" $ do
    it "creates an empty array of the desired size" $
      quickCheck \sz ->
        let posSz = abs sz `mod` 10000
        in  (unsafePartial $ fromJust
             $ pureST (traverse unsafeFreeze =<< newSTTypedArray posSz)
            )
            `eq` (unsafePartial $ fromJust $ newTypedArray posSz) :: Int8Array
