module HKT.Type (ID, HKT) where

data ID a

type family HKT a b where
    HKT ID a = a
    HKT b a = b a
