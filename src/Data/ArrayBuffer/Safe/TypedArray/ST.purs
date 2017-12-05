module Data.ArrayBuffer.Safe.TypedArray.ST (
  STTypedArray
, thaw
, peek
, poke
, unsafeFreeze
, freeze
) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.ArrayBuffer.Safe.TypedArray.Class (class IsArrayType, Constructor, constructor)
import Data.ArrayBuffer.Types (ArrayView)
import Data.Maybe (Maybe(..))
import Prelude (pure, (<<<))
import Unsafe.Coerce (unsafeCoerce)

foreign import data STTypedArray :: Type -> Type -> Type

newSTTypedArray
  :: forall t m h r
   . IsArrayType t m
  => Int
  -> Eff (st :: ST h | r) (Maybe (STTypedArray t h))
newSTTypedArray = newSTTypedArrayImpl Just Nothing constructor

foreign import newSTTypedArrayImpl
  :: forall t h r
   . (STTypedArray t h -> Maybe (STTypedArray t h)) 
  -> Maybe (STTypedArray t h)
  -> Constructor t
  -> Int
  -> Eff (st :: ST h | r) (Maybe (STTypedArray t h))

-- | Read the value at the specified index in a mutable array.
peek
  :: forall t m h r
   . IsArrayType (ArrayView t) m
  => STTypedArray h (ArrayView t)
  -> Int
  -> Eff (st :: ST h | r) (Maybe m)
peek = peekImpl Just Nothing

foreign import peekImpl
  :: forall t m h e r
   . IsArrayType (ArrayView t) m
  => (m -> r)
  -> r
  -> STTypedArray h (ArrayView t)
  -> Int
  -> (Eff (st :: ST h | e) r)

-- | Change the value at the specified index in a mutable array.
foreign import poke
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
