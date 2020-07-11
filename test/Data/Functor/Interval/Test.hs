{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Data.Semigroup as S ((<>))
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: IO Bool
tests = checkParallel $$(discover)

prop_point_membership = property $ do
  p <- pure <$> forAll gp
  assert $ p `member` (point p :: Interval Identity Int)


prop_toUnit_infimum = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  toUnit i (inf i) === 0

prop_toUnit_supremum = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  toUnit i (sup i) === 1

prop_toUnit_inverse = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  p <- pure <$> forAll gf
  toUnit i (fromUnit i p) === p


prop_fromUnit_infimum = property $ do
  i <- forAll (interval gf)
  fromUnit i 0 === inf i

prop_fromUnit_supremum = property $ do
  i <- forAll (interval gf)
  fromUnit i 1 === sup i

prop_fromUnit_inverse = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  p <- pure <$> forAll gf
  fromUnit i (toUnit i p) === p


prop_transform_infimum = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  j <- forAll (interval gf) >>= forAll . properSuperinterval
  transform i j (inf i) === inf j

prop_transform_midpoint = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  j <- forAll (interval gf) >>= forAll . properSuperinterval
  transform i j (midpoint i) === midpoint j

prop_transform_supremum = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  j <- forAll (interval gf) >>= forAll . properSuperinterval
  transform i j (sup i) === sup j

prop_transform_identity = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  p <- pure <$> forAll gf
  transform i i p === p

prop_transform_inverse = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  j <- forAll (interval gf) >>= forAll . properSuperinterval
  p <- pure <$> forAll gf
  transform i j (transform j i p) === p

prop_transform_fromUnit = property $ do
  i <- forAll (interval gf)
  p <- pure <$> forAll gf
  transform (0...1) i p === fromUnit i p

prop_transform_toUnit = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  p <- pure <$> forAll gf
  transform i (0...1) p === toUnit i p


prop_lerp_infimum = property $ do
  i <- forAll gi
  lerp 0 i === inf i

prop_lerp_supremum = property $ do
  i <- forAll gi
  lerp 1 i === sup i

prop_lerp_midpoint = property $ do
  i <- forAll (interval gf)
  lerp 0.5 i === midpoint i

prop_lerp_fromUnit = property $ do
  i <- forAll (interval gf)
  t <- forAll gf
  lerp t i === fromUnit i (pure t)


prop_wrap_lerp = property $ do
  i <- forAll (interval gf) >>= forAll . properSuperinterval
  t <- forAll gf
  wrap i (lerp t i) === lerp (snd (properFraction t :: (Integer, Rational))) i


prop_member_infimum = property $ do
  i <- forAll gi
  assert $ inf i `member` i

prop_member_supremum = property $ do
  i <- forAll gi
  assert $ sup i `member` i


prop_isValid_point = property $ do
  p <- pure <$> forAll gp
  assert $ isValid (point p :: Interval Identity Int)


prop_isSubintervalOf_reflexivity = property $ do
  i <- forAll gi
  assert $ i `isSubintervalOf` i

prop_isSubintervalOf_transitivity = property $ do
  (i1, i2, i3) <- forAll gi >>= \ i1 -> forAll (superinterval i1) >>= \ i2 -> (,,) i1 i2 <$> forAll (superinterval i2)
  label $ (if i1 == i2 then "i1 = i2" else "i1 ⊂ i2") S.<> " ∧ " S.<> (if i2 == i3 then "i2 = i3" else "i2 ⊂ i3")
  assert (i1 `isSubintervalOf` i3)

prop_isSubintervalOf_offset = property $ do
  i <- forAll gi
  d <- forAll nonZeroDelta
  assert . not $ i `isSubintervalOf` mapInterval (+ d) i
  assert . not $ mapInterval (+ d) i `isSubintervalOf` i


prop_isSuperintervalOf_reflexivity = property $ do
  i <- forAll gi
  assert $ i `isSuperintervalOf` i

prop_isSuperintervalOf_transitivity = property $ do
  (i1, i2, i3) <- forAll gi >>= \ i1 -> forAll (superinterval i1) >>= \ i2 -> (,,) i1 i2 <$> forAll (superinterval i2)
  label $ (if i1 == i2 then "i1 = i2" else "i1 ⊂ i2") S.<> " ∧ " S.<> (if i2 == i3 then "i2 = i3" else "i2 ⊂ i3")
  assert (i3 `isSuperintervalOf` i1)

prop_isSuperintervalOf_offset = property $ do
  i <- forAll gi
  d <- forAll nonZeroDelta
  assert . not $ i `isSuperintervalOf` mapInterval (+ d) i
  assert . not $ mapInterval (+ d) i `isSuperintervalOf` i


prop_isProperSubintervalOf_antireflexivity = property $ do
  i <- forAll gi
  assert . not $ i `isProperSubintervalOf` i

prop_isProperSubintervalOf_transitivity = property $ do
  (i1, i3) <- forAll gi >>= \ i1 -> forAll (properSuperinterval i1) >>= \ i2 -> (,) i1 <$> forAll (properSuperinterval i2)
  assert (i1 `isProperSubintervalOf` i3)

prop_isProperSubintervalOf_offset = property $ do
  i <- forAll gi
  d <- forAll nonZeroDelta
  assert . not $ i `isProperSubintervalOf` mapInterval (+ d) i
  assert . not $ mapInterval (+ d) i `isProperSubintervalOf` i


prop_isProperSuperintervalOf_antireflexivity = property $ do
  i <- forAll gi
  assert . not $ i `isProperSuperintervalOf` i

prop_isProperSuperintervalOf_transitivity = property $ do
  (i1, i3) <- forAll gi >>= \ i1 -> forAll (properSuperinterval i1) >>= \ i2 -> (,) i1 <$> forAll (properSuperinterval i2)
  assert (i3 `isProperSuperintervalOf` i1)

prop_isProperSuperintervalOf_offset = property $ do
  i <- forAll gi
  d <- forAll nonZeroDelta
  assert . not $ i `isProperSuperintervalOf` mapInterval (+ d) i
  assert . not $ mapInterval (+ d) i `isProperSuperintervalOf` i


prop_intersects_reflexivity = property $ do
  i <- forAll gi
  assert $ i `intersects` i

prop_intersects_symmetry = property $ do
  i <- forAll gi
  j <- forAll gi
  i `intersects` j === j `intersects` i


prop_union_idempotence = property $ do
  i <- forAll gi
  i `union` i === i

prop_union_associativity = property $ do
  (i1, i2, i3) <- forAll ((,,) <$> gi <*> gi <*> gi)
  (i1 `union` i2) `union` i3 === i1 `union` (i2 `union` i3)

prop_union_commutativity = property $ do
  (i1, i2) <- forAll ((,) <$> gi <*> gi)
  i1 `union` i2 === (i2 `union` i1)


prop_intersection_idempotence = property $ do
  i <- forAll gi
  i `intersection` i === i

prop_intersection_associativity = property $ do
  (i1, i2, i3) <- forAll ((,,) <$> gi <*> gi <*> gi)
  (i1 `intersection` i2) `intersection` i3 === i1 `intersection` (i2 `intersection` i3)

prop_intersection_commutativity = property $ do
  (i1, i2) <- forAll ((,) <$> gi <*> gi)
  i1 `intersection` i2 === (i2 `intersection` i1)


prop_interval_validity = property (forAll gi >>= assert . isValid)

prop_interval_coverage = verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
  i <- forAll gi
  cover 20 "point" (isPoint i)
  cover 20 "span" (inf i < sup i)


prop_superinterval_validity = property (forAll gi >>= forAll . superinterval >>= assert . isValid)

prop_superinterval_correctness = property (forAll gi >>= \ i -> forAll (superinterval i) >>= assert . isSubintervalOf i)

prop_superinterval_coverage = verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
  i <- forAll gi
  si <- forAll (superinterval i)
  cover 20 "=" (i == si)
  cover 10 "⊃" (i `isProperSubintervalOf` si)
  cover 20 "point" (isPoint si)
  cover 20 "span" (inf si < sup si)


prop_properSuperinterval_validity = property (forAll gi >>= forAll . properSuperinterval >>= assert . isValid)

prop_properSuperinterval_correctness = property (forAll gi >>= \ i -> forAll (properSuperinterval i) >>= assert . isProperSubintervalOf i)

prop_properSuperinterval_coverage = verifiedTermination . withConfidence (10^(6 :: Int)) . property $ do
  i <- forAll gi
  si <- forAll (properSuperinterval i)
  cover 20 "inf si == inf i" $ inf si == inf i
  cover 20 "inf si <  inf i" $ inf si <  inf i
  cover 20 "sup si == sup i" $ sup si == sup i
  cover 20 "sup si >  sup i" $ sup si >  sup i


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
