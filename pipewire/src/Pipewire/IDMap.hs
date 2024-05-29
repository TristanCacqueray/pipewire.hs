-- | A simple wrapper around IntMap to store pwid base object
module Pipewire.IDMap where

import Data.Coerce (coerce)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM

import Pipewire.Protocol (PwID (..))

newtype IDMap a = IDMap (IntMap a)
    deriving newtype (Semigroup, Monoid)

toList :: IDMap a -> [(PwID, a)]
toList (IDMap im) = coerce $ IM.toList im

insert :: PwID -> a -> IDMap a -> IDMap a
insert (PwID pwid) v (IDMap im) = IDMap $ IM.insert pwid v im

delete :: PwID -> IDMap a -> IDMap a
delete (PwID pwid) (IDMap im) = IDMap $ IM.delete pwid im

lookup :: PwID -> IDMap a -> Maybe a
lookup (PwID pwid) (IDMap im) = IM.lookup pwid im
