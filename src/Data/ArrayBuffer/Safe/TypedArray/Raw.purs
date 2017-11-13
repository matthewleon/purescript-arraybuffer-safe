module Data.ArrayBuffer.Safe.TypedArray.Raw (
  every
, filter
, find
, findIndex
, includes
, indexOf
, lastIndexOf
, map
, reduce
, reduceRight
, slice
, some
, subarray
--TODO (typing is a bit hairy), toLocaleString
, toString
) where 

import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.Safe.TypedArray.Class (class IsArrayType)

foreign import every :: forall t m. IsArrayType t m => (m -> Int -> t -> Boolean) -> t -> Boolean

foreign import filter :: forall t m. IsArrayType t m => (m -> Int -> t -> Boolean) -> t -> t

foreign import find :: forall t m. IsArrayType t m => (m -> Int -> t -> Boolean) -> t -> m

foreign import findIndex :: forall t m. IsArrayType t m => (m -> Int -> t -> Boolean) -> t -> Int

-- not supported in all browsers
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/includes
foreign import includes :: forall t m. IsArrayType t m => m -> Int -> t -> Boolean

foreign import indexOf :: forall t m. IsArrayType t m => m -> t -> Int

foreign import lastIndexOf :: forall t m. IsArrayType t m => m -> t -> Int

foreign import join :: forall t. String -> ArrayView t -> String

foreign import map :: forall t m. IsArrayType t m => (m -> Int -> t -> m) -> t -> t

foreign import reduce :: forall t m a. IsArrayType t m => (a -> m -> Int -> t -> a) -> a -> t -> a

foreign import reduceRight :: forall t m a. IsArrayType t m => (a -> m -> Int -> t -> a) -> a -> t -> a

-- you probably want subarray, which doesn't result in copying, instead
foreign import slice :: forall t. Int -> Int -> ArrayView t -> ArrayView t

foreign import some :: forall t m. IsArrayType t m => (m -> Int -> t -> Boolean) -> t -> Boolean

-- you probably want subarray, which doesn't result in copying, instead
foreign import subarray :: forall t. Int -> Int -> ArrayView t -> ArrayView t

foreign import toString :: forall t. ArrayView t -> String
