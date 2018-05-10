module HKT.Merge (Merge, merge) where

import Protolude

import HKT.Type (ID)
import GHC.Generics (Generic, Rep, M1(M1), K1(K1), V1, U1, from, to, (:*:)((:*:)))
import qualified Data.Maybe

class Merge a where
    merge :: a ID -> a Maybe -> a ID
    default merge ::
        ( Generic (a Maybe)
        , Generic (a ID)
        , GMerge (Rep (a ID)) (Rep (a Maybe))
        ) => a ID -> a Maybe -> a ID
    merge a b = to $ gmerge (from a ) (from b)

class GMerge a b where
    gmerge :: a p -> b p -> a p

instance GMerge a b => GMerge (M1 i c a) (M1 i c b) where
    gmerge (M1 a) (M1 b) = M1 $ gmerge a b
    {-# INLINE gmerge #-}

instance (GMerge a b, GMerge c d) => GMerge (a :*: c) (b :*: d) where
    gmerge (a :*: c) (b :*: d) = gmerge a b :*: gmerge c d
    {-# INLINE gmerge #-}

instance GMerge (K1 a k) (K1 a (Maybe k)) where
    gmerge (K1 a) (K1 ma) = K1 $ Data.Maybe.fromMaybe a ma
    {-# INLINE gmerge #-}

instance GMerge U1 U1 where
    gmerge U1 _ = U1
    {-# INLINE gmerge #-}

instance GMerge V1 V1 where
    gmerge _ = (\case {})
    {-# INLINE gmerge #-}
