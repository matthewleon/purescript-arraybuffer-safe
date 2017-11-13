module Data.ArrayBuffer.Safe.DataView.ST (
  STDataView
, Getter
, Setter

, fromArrayBuffer
, buffer
, byteLength
, byteOffset

, getInt8
, getInt16be
, getInt16le
, getInt32be
, getInt32le
, getUint8
, getUint16be
, getUint16le
, getUint32le
, getUint32be
, getFloat32be
, getFloat32le
, getFloat64be
, getFloat64le

, setInt8
, setInt16be
, setInt16le
, setInt32be
, setInt32le
, setUint8
, setUint16be
, setUint16le
, setUint32le
, setUint32be
, setFloat32be
, setFloat32le
, setFloat64be
, setFloat64le
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.ArrayBuffer.Safe.ArrayBuffer (ArrayBuffer, ByteLength)
import Data.ArrayBuffer.Safe.DataView as DV
import Data.ArrayBuffer.Types (ByteOffset)
import Data.Maybe (Maybe)
import Data.UInt (UInt)
import Unsafe.Coerce (unsafeCoerce)

foreign import data STDataView :: Type -> Type

-- | Type for all fetching functions.
type Getter a =
     forall h r
   . STDataView h
  -> ByteOffset
  -> Eff (st :: ST h | r) (Maybe a)

-- | Type for all setting functions. Boolean result indicates success.
type Setter a =
     forall h r
   . STDataView h
  -> ByteOffset
  -> a
  -> Eff (st :: ST h | r) Boolean

foreign import fromArrayBuffer :: forall h r. ArrayBuffer -> Eff (st :: ST h | r) (STDataView h)

buffer :: forall h r. STDataView h -> Eff (st :: ST h | r) ArrayBuffer
buffer = liftFromDV DV.buffer

byteLength :: forall h r. STDataView h -> Eff (st :: ST h | r) ByteLength
byteLength = liftFromDV DV.byteLength

byteOffset :: forall h r. STDataView h -> Eff (st :: ST h | r) ByteOffset
byteOffset = liftFromDV DV.byteOffset

getInt8 :: Getter Int
getInt8 = useImmutableGetter DV.getInt8

getInt16be :: Getter Int
getInt16be = useImmutableGetter DV.getInt16be

getInt16le :: Getter Int
getInt16le = useImmutableGetter DV.getInt16le

getInt32be :: Getter Int
getInt32be = useImmutableGetter DV.getInt32be

getInt32le :: Getter Int
getInt32le = useImmutableGetter DV.getInt32le

getUint8 :: Getter UInt
getUint8 = useImmutableGetter DV.getUint8

getUint16be :: Getter UInt
getUint16be = useImmutableGetter DV.getUint16be

getUint16le :: Getter UInt
getUint16le = useImmutableGetter DV.getUint16le

getUint32be :: Getter UInt
getUint32be = useImmutableGetter DV.getUint32be

getUint32le :: Getter UInt
getUint32le = useImmutableGetter DV.getUint32le

getFloat32be :: Getter Number
getFloat32be = useImmutableGetter DV.getFloat32be

getFloat32le :: Getter Number
getFloat32le = useImmutableGetter DV.getFloat32le

getFloat64be :: Getter Number
getFloat64be = useImmutableGetter DV.getFloat64be

getFloat64le :: Getter Number
getFloat64le = useImmutableGetter DV.getFloat64le

setInt8 :: Setter Int
setInt8 = setter "setInt8" false

setInt16be :: Setter Int
setInt16be = setter "setInt16" false

setInt16le :: Setter Int
setInt16le = setter "setInt16" true

setInt32be :: Setter Int
setInt32be = setter "setInt32" false

setInt32le :: Setter Int
setInt32le = setter "setInt32" true

setUint8 :: Setter UInt
setUint8 = setter "setUint8" false

setUint16be :: Setter UInt
setUint16be = setter "setUint16" false

setUint16le :: Setter UInt
setUint16le = setter "setUint16" true

setUint32be :: Setter UInt
setUint32be = setter "setUint32" false

setUint32le :: Setter UInt
setUint32le = setter "setUint32" true

setFloat32be :: Setter Number
setFloat32be = setter "setFloat32" false

setFloat32le :: Setter Number
setFloat32le = setter "setFloat32" true

setFloat64be :: Setter Number
setFloat64be = setter "setFloat64" false

setFloat64le :: Setter Number
setFloat64le = setter "setFloat64" true

liftFromDV
  :: forall a
   . (DV.DataView -> a)
  -> forall h r. STDataView h -> Eff (st :: ST h | r) a
liftFromDV f = pure <<< f <<< unsafeCoerce

useImmutableGetter :: forall a. DV.Getter a -> Getter a
useImmutableGetter g dv = pure <<< g (unsafeCoerce dv)

type Endianness = Boolean

foreign import setter :: forall a. String -> Endianness -> Setter a
