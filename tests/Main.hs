{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE CPP               #-}

module Main where

import Protolude hiding (Product)

import GHC.Generics (Generic)
import Data.Text (Text)
import Test.Hspec

#ifdef INSPECTION_TESTING
import Test.Inspection (inspect, (===))
import qualified Data.Maybe
#endif

import HKT (HKT, ID, Merge, Squash, squash, merge)

data Empty (a :: * -> *)
    deriving stock Generic
    deriving anyclass (Squash, Merge) 
data One (a :: * -> *) = One
    deriving stock Generic
    deriving anyclass (Squash, Merge)

data Product (a :: * -> *) =
    Product
    { first' :: HKT a Int
    , second' :: HKT a Text
    }
    deriving stock Generic
    deriving anyclass (Squash, Merge)

deriving instance Show (Product ID)
deriving instance Eq (Product ID)

data SumType (a :: * -> *) 
    = SumLeft (HKT a Int) 
    | SumRight (HKT a Text)
    deriving stock Generic
    deriving anyclass Squash

#ifdef INSPECTION_TESTING
squash1, squash2 :: Product Maybe -> Maybe (Product ID)
squash1 = squash
squash2 Product { first', second' } = 
    Product <$> first' <*> second'

inspect $ 'squash1 === 'squash2

merge1, merge2 :: Product ID -> Product Maybe -> Product ID
merge1 = merge
merge2 (Product pf pl) (Product p1f p1l) =
    Product
        (Data.Maybe.fromMaybe pf p1f)
        (Data.Maybe.fromMaybe pl p1l)

inspect $ 'merge1 === 'merge2

squash3, squash4 :: SumType Maybe -> Maybe (SumType ID)
squash3 = squash
squash4 s =
  case s of
    SumLeft (Just x) -> Just (SumLeft x)
    SumRight (Just x) -> Just (SumRight x)
    _ -> Nothing

inspect $ 'squash3 === 'squash4
#endif

main :: IO ()
main = 
    let
        p :: Product ID
        p = Product 5 "hello"

        p1 :: Product Maybe
        p1 = Product (Just 5) (Just "hello")
    in
    hspec $ do
        describe "Squash" $ do
            it "squashes fields properly" $ do
                squash p1 `shouldBe` Just p
            it "returns nothing when missing fields" $ do
                squash p1{first' = Nothing} `shouldBe` Nothing
        describe "Merge" $ do
            it "merges fields properly" $ do
                merge p p1 `shouldBe` p
            it "takes fields from the second argument when avaliable" $ do
                merge p{first' = 6} p1 `shouldBe` p
            it "takes fields from the first argument when missing in the second" $ do
                merge p p1{first' = Nothing, second' = Just "meh"} `shouldBe` p{second' = "meh"}
