module Test.QuickCheck.Instances.Functor.Interval where

import Data.Functor.Interval (Interval (Interval))
import Test.QuickCheck (Arbitrary, arbitrary)

instance (Ord a, Applicative f, Arbitrary (f a)) => Arbitrary (Interval f a) where
  arbitrary = pure Interval <*> arbitrary <*> arbitrary
