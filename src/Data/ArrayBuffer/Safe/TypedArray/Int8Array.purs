module Data.ArrayBuffer.Safe.TypedArray.Int8Array (
  fromArrayBuffer
, module Data.ArrayBuffer.Types
) where

import Data.ArrayBuffer.Types (ArrayBuffer, Int8Array)

foreign import fromArrayBuffer :: ArrayBuffer -> Int8Array
