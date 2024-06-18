-- | A simple wrapper around IntMap to store pwid base object
module Pipewire.IDMap where

import Data.Coerce (coerce)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Prelude hiding (filter)

import Pipewire.Protocol (PwID (..))

newtype IDMap a = IDMap (IntMap a)
    deriving newtype (Semigroup, Monoid)

toList :: IDMap a -> [(PwID, a)]
toList (IDMap im) = coerce $ IM.toList im

insert :: PwID -> a -> IDMap a -> IDMap a
insert (PwID pwid) v (IDMap im) = IDMap $ IM.insert pwid v im

filter :: (a -> Bool) -> IDMap a -> IDMap a
filter pred' (IDMap im) = IDMap $ IM.filter pred' im

delete :: PwID -> IDMap a -> IDMap a
delete (PwID pwid) (IDMap im) = IDMap $ IM.delete pwid im

lookup :: PwID -> IDMap a -> Maybe a
lookup (PwID pwid) (IDMap im) = IM.lookup pwid im

contains :: (a -> Bool) -> IDMap a -> Bool
contains pred' (IDMap im) = not $ IM.null $ IM.filter pred' im

find :: (a -> Bool) -> IDMap a -> Maybe (PwID, a)
find pred' im = case toList (filter pred' im) of
    (x : _) -> Just x
    _ -> Nothing

keep :: (a -> Bool) -> IDMap a -> [(PwID, a)]
keep pred' = toList . filter pred'

foldl' :: (a -> (PwID, b) -> a) -> a -> IDMap b -> a
foldl' go i (IDMap im) = IM.foldlWithKey' go' i im
  where
    go' a k b = go a (PwID k, b)
