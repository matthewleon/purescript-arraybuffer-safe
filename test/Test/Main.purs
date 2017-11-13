module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Data.ArrayBuffer.Safe.ArrayBuffer (testArrayBuffer)
import Test.Data.ArrayBuffer.Safe.DataView (testDataView)
import Test.Data.ArrayBuffer.Safe.DataView.ST (testSTDataView)
import Test.Data.ArrayBuffer.Safe.TypedArray (testTypedArray)
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  testArrayBuffer
  testDataView
  testSTDataView
  testTypedArray
