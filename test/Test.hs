module Main
( main
) where

import qualified Data.Functor.Interval.Test as Interval
import           Hedgehog.Main

main :: IO ()
main = defaultMain Interval.tests
