module Data.ArrayBuffer.Safe.ArrayBuffer (
  byteLength
, slice
--, isView
, eq
, notEq
, module Data.ArrayBuffer.Types
) where

import Prelude hiding (eq, notEq)

import Data.Function.Uncurried (Fn3, runFn3)
import Data.ArrayBuffer.Safe.TypedArray as TA
import Data.ArrayBuffer.Safe.TypedArray.Int8Array as I8A
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)

-- | Represents the length of an `ArrayBuffer` in bytes.
foreign import byteLength :: ArrayBuffer -> ByteLength

foreign import sliceImpl :: Fn3 ByteOffset Int ArrayBuffer ArrayBuffer

-- | Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.
slice :: ByteOffset -> Int -> ArrayBuffer -> ArrayBuffer
slice = runFn3 sliceImpl

eq :: ArrayBuffer -> ArrayBuffer -> Boolean
eq ab1 ab2 =
  byteLength ab1 == byteLength ab2
  -- optimization? choose a higher-byte TypedArray when byteLength permits
  && I8A.fromArrayBuffer ab1 `TA.eq` I8A.fromArrayBuffer ab2

notEq :: ArrayBuffer -> ArrayBuffer -> Boolean
notEq ab1 ab2 = not $ ab1 `eq` ab2
