module Data.ArrayBuffer.Safe.TypedArray.Uint8Array (
  fromArrayBuffer
, module Data.ArrayBuffer.Types
) where

import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)

foreign import fromArrayBuffer :: ArrayBuffer -> Uint8Array
