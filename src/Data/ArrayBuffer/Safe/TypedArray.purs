module Data.ArrayBuffer.Safe.TypedArray (
  newTypedArray
, fromArray
, fromArrayBuffer
, fromArrayBufferWithOffset
, fromArrayBufferWithOffsetAndLength
, fromTypedArray
, buffer
, byteLength
, byteOffset
, length

--
--, toUnfoldable
--, fromFoldable
, empty
, singleton
--, range
--, replicate
--, (..)
--, some
--, many
--, null
--, cons
--, (:)
--, snoc
--, insert
--, head
--, tail
--, init
--, uncons
--, unsnoc
, index
, (!!)
--,elemIndex
--,elemLastIndex
--,findIndex
--,findLastIndex
--,insertAt
--,deleteAt
--,updateAt
--,modifyAt
--,alterAt
--,reverse
--,concat
--,concatMap
, filter
--,partition
--,filterA
--,mapMaybe
--,catMaybes
--,mapWithIndex
--,updateAtIndices
--,modifyAtIndices
--,sort
--,sortBy
--,sortWith
, slice
, take
--,takeWhile
, drop
--,dropWhile
--,span
--,group
--,group'
--,groupBy
--,nub
--,nubBy
--,union
--,unionBy
--,delete
--,deleteBy
--,difference
--,(\\)
--,intersect
--,intersectBy
--,zipWith
--,zipWithA
--,zip
--,unzip
--,foldM
--,foldRecM
, unsafeIndex


-- typeclassy methods
, show
, eq
, notEq
, every
, map
, foldl
--, bytesPerElement

, module Data.ArrayBuffer.Types
, module Data.ArrayBuffer.Safe.TypedArray.Class
) where

import Prelude hiding (eq, notEq)

import Data.Array as A
import Data.ArrayBuffer.Safe.TypedArray.Class (class IsArrayType, Constructor, constructor)
import Data.ArrayBuffer.Safe.TypedArray.Raw as Raw
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, ByteOffset, ByteLength, Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

newTypedArray
  :: forall t m
   . IsArrayType t m
  => Int
  -> Maybe t
newTypedArray = newTypedArrayImpl Just Nothing constructor

foreign import newTypedArrayImpl
  :: forall t
   . (t -> Maybe t)
  -> Maybe t
  -> Constructor t
  -> Int
  -> Maybe t

foreign import fromArray :: forall t m. IsArrayType t m => Array m -> t

-- TODO: This returns t, rather than Maybe t, for 8-bit arrays
fromArrayBuffer :: forall t m. IsArrayType t m => ArrayBuffer -> Maybe t
fromArrayBuffer = fromArrayBufferImpl Just Nothing

foreign import fromArrayBufferImpl
  :: forall t m
   . IsArrayType t m
  => (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> ArrayBuffer
  -> Maybe t

fromArrayBufferWithOffset
  :: forall t m
   . IsArrayType t m
  => ArrayBuffer
  -> ByteOffset
  -> Maybe t
fromArrayBufferWithOffset = fromArrayBufferWithOffsetImpl Just Nothing

foreign import fromArrayBufferWithOffsetImpl
  :: forall t m
   . IsArrayType t m
  => (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> ArrayBuffer
  -> ByteOffset
  -> Maybe t

fromArrayBufferWithOffsetAndLength
  :: forall t m
   . IsArrayType t m
  => ArrayBuffer
  -> ByteOffset
  -> Int
  -> Maybe t
fromArrayBufferWithOffsetAndLength =
  fromArrayBufferWithOffsetAndLengthImpl Just Nothing

foreign import fromArrayBufferWithOffsetAndLengthImpl
  :: forall t m
   . IsArrayType t m
  => (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> ArrayBuffer
  -> ByteOffset
  -> Int
  -> Maybe t

foreign import fromTypedArray :: forall t t'. ArrayView t -> ArrayView t'

foreign import buffer :: forall t. ArrayView t -> ArrayBuffer

foreign import byteLength :: forall t. ArrayView t -> ByteLength

foreign import byteOffset :: forall t. ArrayView t -> Int

foreign import length :: forall t. ArrayView t -> Int

-- | Create an empty typed array.
foreign import empty :: forall t m. IsArrayType t m => t

-- | Create a typed array of one element
singleton :: forall t m. IsArrayType t m => m -> t
singleton = fromArray <<< A.singleton

index :: forall t m. IsArrayType t m => t -> Int -> Maybe m
index = A.index <<< unsafeCoerce

-- | An infix version of `index`.
infixl 8 index as !!

show :: forall t. ArrayView t -> String
show xs = "fromArray [" <> Raw.toString xs <> "]"

eq :: forall t m. IsArrayType (ArrayView t) m => Eq m => ArrayView t -> ArrayView t -> Boolean
eq xs ys = byteLength xs == byteLength ys && flip Raw.every xs \x i _ ->
  x == unsafePartial (fromJust $ ys !! i)

notEq :: forall t m. IsArrayType (ArrayView t) m => Eq m => ArrayView t -> ArrayView t -> Boolean
notEq xs ys = xs `eq` ys == false

foreign import every :: forall t m. IsArrayType t m => (m -> Boolean) -> t -> Boolean

foreign import filter :: forall t m. IsArrayType t m => (m -> Boolean) -> t -> t

foreign import slice :: forall t. ArrayView t -> Int -> Int -> ArrayView t

take :: forall t. ArrayView t -> Int -> ArrayView t
take xs = slice xs 0

foreign import drop :: forall t. ArrayView t -> Int -> ArrayView t

foreign import map :: forall t m. IsArrayType t m => (m -> m) -> t -> t

foreign import foldl :: forall t m a. IsArrayType t m => (a -> m -> a) -> a -> t -> a

-- | Find the element of an array at the specified index.
unsafeIndex :: forall t m. IsArrayType t m => Partial => t -> Int -> m
unsafeIndex = unsafeIndexImpl

foreign import unsafeIndexImpl :: forall t m. IsArrayType t m => t -> Int -> m

-- TODO
--foreign import bytesPerElement :: forall t m. IsArrayType t m => Int
