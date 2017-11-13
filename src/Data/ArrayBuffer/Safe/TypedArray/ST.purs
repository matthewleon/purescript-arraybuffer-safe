module Data.ArrayBuffer.Safe.TypedArray.ST (
  STTypedArray
, thaw
, peekSTTypedArray
, pokeSTTypedArray
, unsafeFreeze
, freeze
) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.ArrayBuffer.Safe.TypedArray (class IsArrayType)
import Data.ArrayBuffer.Types (ArrayView)
import Data.Maybe (Maybe(..))
import Prelude (pure, (<<<))
import Unsafe.Coerce (unsafeCoerce)

foreign import data STTypedArray :: Type -> Type -> Type

-- | Read the value at the specified index in a mutable array.
peekSTTypedArray
  :: forall t m h r
   . IsArrayType (ArrayView t) m
  => STTypedArray h (ArrayView t)
  -> Int
  -> Eff (st :: ST h | r) (Maybe m)
peekSTTypedArray = peekSTTypedArrayImpl Just Nothing

foreign import peekSTTypedArrayImpl
  :: forall t m h e r
   . IsArrayType (ArrayView t) m
  => (m -> r)
  -> r
  -> STTypedArray h (ArrayView t)
  -> Int
  -> (Eff (st :: ST h | e) r)

-- | Change the value at the specified index in a mutable array.
foreign import pokeSTTypedArray
  :: forall t m h r
   . IsArrayType (ArrayView t) m
  => STTypedArray h (ArrayView t)
  -> Int
  -> m
  -> Eff (st :: ST h | r) Boolean

-- | O(1). Convert a mutable array to an immutable array, without copying. The mutable
-- | array must not be mutated afterwards.
unsafeFreeze :: forall t h r. STTypedArray h (ArrayView t) -> Eff (st :: ST h | r) (ArrayView t)
unsafeFreeze = pure <<< (unsafeCoerce :: STTypedArray h (ArrayView t) -> ArrayView t)

-- | Create an immutable copy of a mutable array.
freeze :: forall t h r. STTypedArray h (ArrayView t) -> Eff (st :: ST h | r) (ArrayView t)
freeze = copyImpl

-- | Create a mutable copy of an immutable typed array.
thaw :: forall t h r. ArrayView t -> Eff (st :: ST h | r) (STTypedArray h (ArrayView t))
thaw = copyImpl

foreign import copyImpl :: forall a b h r. a -> Eff (st :: ST h | r) b
