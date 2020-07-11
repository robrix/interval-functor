{-# LANGUAGE OverloadedStrings #-}
module Data.Functor.Interval.Test
( tests
, interval
, superinterval
, properSuperinterval
, delta
, nonZeroDelta
) where

import           Control.Monad (join)
import           Data.Function ((&))
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: [IO Bool]
tests = map checkParallel
  [ Group "point"
    [ (,) "membership" $ property $ do
      p <- pure <$> forAll gp
      assert $ p `member` (point p :: Interval Identity Int)
    ]

  , Group "toUnit"
    [ (,) "infimum" $ property $ do
      i <- forAll (interval gf) >>= forAll . properSuperinterval
      toUnit i (inf i) === 0
    , (,) "supremum" $ property $ do
      i <- forAll (interval gf) >>= forAll . properSuperinterval
      toUnit i (sup i) === 1
    , (,) "inverse" $ property $ do
      i <- forAll (interval gf) >>= forAll . properSuperinterval
      p <- pure <$> forAll gf
      toUnit i (fromUnit i p) === p
    ]

  , Group "fromUnit"
    [ (,) "infimum" $ property $ do
      i <- forAll (interval gf)
      fromUnit i 0 === inf i
    , (,) "supremum" $ property $ do
      i <- forAll (interval gf)
      fromUnit i 1 === sup i
    , (,) "inverse" $ property $ do
      i <- forAll (interval gf) >>= forAll . properSuperinterval
      p <- pure <$> forAll gf
      fromUnit i (toUnit i p) === p
    ]

  , Group "transform"
    [ (,) "identity" $ property $ do
      i <- forAll (interval gf) >>= forAll . properSuperinterval
      p <- pure <$> forAll gf
      transform i i p === p
    , (,) "fromUnit" $ property $ do
      i <- forAll (interval gf)
      p <- pure <$> forAll gf
      transform (0...1) i p === fromUnit i p
    ]

  , Group "lerp"
    [ (,) "infimum" $ property $ do
      i <- forAll gi
      lerp 0 i === inf i
    , (,) "supremum" $ property $ do
      i <- forAll gi
      lerp 1 i === sup i
    , (,) "midpoint" $ property $ do
      i <- forAll (interval gf)
      lerp 0.5 i === midpoint i
    ]

  , Group "member"
    [ (,) "infimum" $ property $ do
      i <- forAll gi
      assert $ inf i `member` i
    , (,) "supremum" $ property $ do
      i <- forAll gi
      assert $ sup i `member` i
    ]

  , Group "isValid"
    [ (,) "point" $ property $ do
      p <- pure <$> forAll gp
      assert $ isValid (point p :: Interval Identity Int)
    ]

  , Group "isSubintervalOf"
    [ (,) "reflexivity" $ property $ do
      i <- forAll gi
      assert $ i `isSubintervalOf` i
    , (,) "transitivity" $ property $ do
      (i1, i2, i3) <- forAll gi >>= \ i1 -> forAll (superinterval i1) >>= \ i2 -> (,,) i1 i2 <$> forAll (superinterval i2)
      label $ (if i1 == i2 then "i1 = i2" else "i1 ⊂ i2") <> " ∧ " <> (if i2 == i3 then "i2 = i3" else "i2 ⊂ i3")
      assert (i1 `isSubintervalOf` i3)
    , (,) "offset" $ property $ do
      i <- forAll gi
      d <- forAll nonZeroDelta
      assert . not $ i `isSubintervalOf` mapInterval (+ d) i
      assert . not $ mapInterval (+ d) i `isSubintervalOf` i
    ]

  , Group "isSuperintervalOf"
    [ (,) "reflexivity" $ property $ do
      i <- forAll gi
      assert $ i `isSuperintervalOf` i
    , (,) "transitivity" $ property $ do
      (i1, i2, i3) <- forAll gi >>= \ i1 -> forAll (superinterval i1) >>= \ i2 -> (,,) i1 i2 <$> forAll (superinterval i2)
      label $ (if i1 == i2 then "i1 = i2" else "i1 ⊂ i2") <> " ∧ " <> (if i2 == i3 then "i2 = i3" else "i2 ⊂ i3")
      assert (i3 `isSuperintervalOf` i1)
    , (,) "offset" $ property $ do
      i <- forAll gi
      d <- forAll nonZeroDelta
      assert . not $ i `isSuperintervalOf` mapInterval (+ d) i
      assert . not $ mapInterval (+ d) i `isSuperintervalOf` i
    ]

  , Group "isProperSubintervalOf"
    [ (,) "antireflexivity" $ property $ do
      i <- forAll gi
      assert . not $ i `isProperSubintervalOf` i
    , (,) "transitivity" $ property $ do
      (i1, i3) <- forAll gi >>= \ i1 -> forAll (properSuperinterval i1) >>= \ i2 -> (,) i1 <$> forAll (properSuperinterval i2)
      assert (i1 `isProperSubintervalOf` i3)
    , (,) "offset" $ property $ do
      i <- forAll gi
      d <- forAll nonZeroDelta
      assert . not $ i `isProperSubintervalOf` mapInterval (+ d) i
      assert . not $ mapInterval (+ d) i `isProperSubintervalOf` i
    ]

  , Group "isProperSuperintervalOf"
    [ (,) "antireflexivity" $ property $ do
      i <- forAll gi
      assert . not $ i `isProperSuperintervalOf` i
    , (,) "transitivity" $ property $ do
      (i1, i3) <- forAll gi >>= \ i1 -> forAll (properSuperinterval i1) >>= \ i2 -> (,) i1 <$> forAll (properSuperinterval i2)
      assert (i3 `isProperSuperintervalOf` i1)
    , (,) "offset" $ property $ do
      i <- forAll gi
      d <- forAll nonZeroDelta
      assert . not $ i `isProperSuperintervalOf` mapInterval (+ d) i
      assert . not $ mapInterval (+ d) i `isProperSuperintervalOf` i
    ]

  , Group "intersects"
    [ (,) "reflexivity" $ property $ do
      i <- forAll gi
      assert $ i `intersects` i
    ]

  , Group "union"
    [ (,) "idempotence" $ property $ do
      i <- forAll gi
      i `union` i === i
    , (,) "associativity" $ property $ do
      (i1, i2, i3) <- forAll ((,,) <$> gi <*> gi <*> gi)
      (i1 `union` i2) `union` i3 === i1 `union` (i2 `union` i3)
    , (,) "commutativity" $ property $ do
      (i1, i2) <- forAll ((,) <$> gi <*> gi)
      i1 `union` i2 === (i2 `union` i1)
    ]

  , Group "intersection"
    [ (,) "idempotence" $ property $ do
      i <- forAll gi
      i `intersection` i === i
    , (,) "associativity" $ property $ do
      (i1, i2, i3) <- forAll ((,,) <$> gi <*> gi <*> gi)
      (i1 `intersection` i2) `intersection` i3 === i1 `intersection` (i2 `intersection` i3)
    , (,) "commutativity" $ property $ do
      (i1, i2) <- forAll ((,) <$> gi <*> gi)
      i1 `intersection` i2 === (i2 `intersection` i1)
    ]

  , Group "interval"
    [ (,) "validity" $ property (forAll gi >>= assert . isValid)
    , (,) "coverage" $ verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      cover 20 "point" (isPoint i)
      cover 20 "span" (inf i < sup i)
    ]

  , Group "superinterval"
    [ (,) "validity" $ property (forAll gi >>= forAll . superinterval >>= assert . isValid)
    , (,) "correctness" $ property (forAll gi >>= \ i -> forAll (superinterval i) >>= assert . isSubintervalOf i)
    , (,) "coverage" $ verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      si <- forAll (superinterval i)
      cover 20 "=" (i == si)
      cover 10 "⊃" (i `isProperSubintervalOf` si)
      cover 20 "point" (isPoint si)
      cover 20 "span" (inf si < sup si)
    ]

  , Group "properSuperinterval"
    [ (,) "validity" $ property (forAll gi >>= forAll . properSuperinterval >>= assert . isValid)
    , (,) "correctness" $ property (forAll gi >>= \ i -> forAll (properSuperinterval i) >>= assert . isProperSubintervalOf i)
    , (,) "coverage" $ verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
      i <- forAll gi
      si <- forAll (properSuperinterval i)
      cover 20 "inf si == inf i" $ inf si == inf i
      cover 20 "inf si <  inf i" $ inf si <  inf i
      cover 20 "sup si == sup i" $ sup si == sup i
      cover 20 "sup si >  sup i" $ sup si >  sup i
    ]
  ]
  where
  gp = Gen.int (Range.linear 0 100)
  gf = Gen.realFrac_ (Range.linearFrac 0 (100 :: Rational))
  gi = interval gp


interval :: (MonadGen m, Num a) => m a -> m (Interval Identity a)
interval p = Gen.choice
  [ join (...) <$> p
  , mk <$> p <*> p
  ]
  where
  mk a b = a ... a + b + 1

superinterval :: (MonadGen m, Num a) => Interval Identity a -> m (Interval Identity a)
superinterval i = do
  l <- delta
  r <- delta
  pure $! Interval (inf i - l) (sup i + r)

properSuperinterval :: (MonadGen m, Num a) => Interval Identity a -> m (Interval Identity a)
properSuperinterval i = Gen.choice
  [ do
    l <- nonZeroDelta
    pure $! i & inf_ -~ l
  , do
    r <- nonZeroDelta
    pure $! i & sup_ +~ r
  , do
    l <- nonZeroDelta
    r <- nonZeroDelta
    pure $! i & inf_ -~ l & sup_ +~ r
  ]

delta :: (MonadGen m, Num a) => m a
delta = Gen.choice [ pure 0, fromIntegral <$> Gen.int (Range.linear 0 10) ]

nonZeroDelta :: (MonadGen m, Num a) => m a
nonZeroDelta = (+ 1) <$> delta

(+~), (-~) :: Num a => ((a -> Identity a) -> s -> Identity t) -> a -> s -> t
l +~ x = runIdentity . l (Identity . (+ x))
l -~ x = runIdentity . l (Identity . subtract x)
infixr 4 +~, -~
