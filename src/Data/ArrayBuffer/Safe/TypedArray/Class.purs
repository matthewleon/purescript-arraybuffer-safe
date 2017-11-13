module Data.ArrayBuffer.Safe.TypedArray.Class (
  class IsArrayType
, Constructor
, constructor
) where

import Data.ArrayBuffer.Types (ArrayView, Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float32Array, Float64Array, Int8, Uint8, Uint8Clamped, Int16, Uint16, Int32, Uint32, Float32, Float64)
import Data.UInt (UInt)

class IsArrayType t m | t -> m where
  constructor :: Constructor t

data Constructor t

instance arrayTypeInt8Array :: IsArrayType (ArrayView Int8) Int
  where constructor = int8ArrayConstructor
instance arrayTypeUint8Array :: IsArrayType (ArrayView Uint8) UInt
  where constructor = uint8ArrayConstructor
instance arrayTypeUint8ClampedArray :: IsArrayType (ArrayView Uint8Clamped) UInt
  where constructor = uint8ClampedArrayConstructor
instance arrayTypeInt16Array :: IsArrayType (ArrayView Int16) Int
  where constructor = int16ArrayConstructor
instance arrayTypeUint16Array :: IsArrayType (ArrayView Uint16) UInt
  where constructor = uint16ArrayConstructor
instance arrayTypeInt32Array :: IsArrayType (ArrayView Int32) Int
  where constructor = int32ArrayConstructor
instance arrayTypeUint32Array :: IsArrayType (ArrayView Uint32) UInt
  where constructor = uint32ArrayConstructor
instance arrayTypeFloat32Array :: IsArrayType (ArrayView Float32) Number
  where constructor = float32ArrayConstructor
instance arrayTypeFloat64Array :: IsArrayType (ArrayView Float64) Number
  where constructor = float64ArrayConstructor

foreign import int8ArrayConstructor :: Constructor Int8Array
foreign import uint8ArrayConstructor :: Constructor Uint8Array
foreign import uint8ClampedArrayConstructor :: Constructor Uint8ClampedArray
foreign import int16ArrayConstructor :: Constructor Int16Array
foreign import uint16ArrayConstructor :: Constructor Uint16Array
foreign import int32ArrayConstructor :: Constructor Int32Array
foreign import uint32ArrayConstructor :: Constructor Uint32Array
foreign import float32ArrayConstructor :: Constructor Float32Array
foreign import float64ArrayConstructor :: Constructor Float64Array
