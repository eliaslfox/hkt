module Main where

import Protolude

import GHC.Generics (Generic)
import Data.Text (Text)

import HKT (HKT, Merge, Squash)

data Empty (a :: * -> *)
    deriving stock Generic
    deriving anyclass (Squash, Merge)

data One (a :: * -> *) = One
    deriving stock Generic
    deriving anyclass (Squash, Merge)

data Product (a :: * -> *) =
    Product
    { first :: HKT a Int
    , second :: HKT a Text
    }
    deriving stock Generic
    deriving anyclass (Squash, Merge)

data Sum (a :: * -> *) 
    = SumLeft (HKT a Int) 
    | SumRight (HKT a Text)
    deriving stock Generic
    deriving anyclass Squash

main :: IO ()
main = pure ()
