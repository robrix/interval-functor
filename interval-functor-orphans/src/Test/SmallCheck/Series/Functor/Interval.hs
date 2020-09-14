{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Test.SmallCheck.Series.Functor.Interval where

import Data.Functor.Interval (Interval (Interval))
import Test.SmallCheck.Series (Serial, series)

instance (Ord a, Applicative f, Serial m (f a)) => Serial m (Interval f a) where
  series =  pure Interval <*> series <*> series
