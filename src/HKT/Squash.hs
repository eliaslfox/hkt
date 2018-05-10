module HKT.Squash (Squash, squash) where

import Protolude

import HKT.Type (ID)
import GHC.Generics (Generic, Rep, M1(M1), K1(K1), V1, U1, from, to, (:*:)((:*:)), (:+:)(L1, R1))

class Squash a where
    squash :: a Maybe -> Maybe (a ID)
    default squash ::
        ( Generic (a Maybe)
        , Generic (a ID)
        , GSquash (Rep (a Maybe)) (Rep (a ID))
        ) => a Maybe -> Maybe (a ID)
    squash x = to <$> gsquash (from x)

class GSquash a b where
    gsquash :: a p -> Maybe (b p)

instance GSquash a b => GSquash (M1 i c a) (M1 i c b) where
    gsquash (M1 x) = M1 <$> gsquash x
    {-# INLINE gsquash #-}

instance (GSquash a b, GSquash c d) => GSquash (a :*: c) (b :*: d) where
    gsquash (a :*: b) = (:*:) <$> gsquash a <*> gsquash b
    {-# INLINE gsquash #-}

instance (GSquash a b, GSquash c d) => GSquash (a :+: c) (b :+: d) where
    gsquash (L1 x) = L1 <$> gsquash x
    gsquash (R1 x) = R1 <$> gsquash x
    {-# INLINE gsquash #-}

instance GSquash (K1 a (Maybe k)) (K1 a k) where
    gsquash (K1 k) = K1 <$> k
    {-# INLINE gsquash #-}

instance GSquash U1 U1 where
    gsquash U1 = Just U1
    {-# INLINE gsquash #-}

instance GSquash V1 V1 where
    gsquash = (\case {})
    {-# INLINE gsquash #-}
