{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Intervals in a space given by an arbitrary functor.
module Data.Functor.Interval
( Interval(..)
  -- * Lenses
, inf_
, sup_
  -- * Constructors
, (...)
, point
  -- * Eliminators
, liftI
, size
, toUnit
, fromUnit
, range
, ranges
, wrap
, imap
  -- * Predicates
, member
, isValid
, isPoint
  -- * Relations
, isSubintervalOf
, isSuperintervalOf
, isProperSubintervalOf
, isProperSuperintervalOf
, before
, after
, Union(..)
, union
, Intersection(..)
, intersection
, intersects
) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Fixed (mod')
import Data.Semigroup
import GHC.Generics (Generic)

-- | @f@-dimensional intervals with coordinates in @a@.
data Interval f a = Interval
  { inf :: !(f a) -- ^ The infimum, or lower bound.
  , sup :: !(f a) -- ^ The supremum, or upper bound.
  }
  deriving (Eq, Foldable, Functor, Generic, Ord, Traversable)

instance Show (f a) => Show (Interval f a) where
  showsPrec p i = showParen (p > 3) $ showsPrec 4 (inf i) . showString "..." . showsPrec 4 (sup i)

instance Applicative f => Applicative (Interval f) where
  pure = point . pure
  {-# INLINE pure #-}
  f <*> a = Interval (inf f <*> inf a) (sup f <*> sup a)
  {-# INLINE (<*>) #-}
  a *> b = Interval (inf a *> inf b) (sup a *> sup b)
  {-# INLINE (*>) #-}
  a <* b = Interval (inf a <* inf b) (sup a <* sup b)
  {-# INLINE (<*) #-}
  liftA2 f a b = Interval (liftA2 f (inf a) (inf b)) (liftA2 f (sup a) (sup b))
  {-# INLINE liftA2 #-}

instance Monad f => Monad (Interval f) where
  m >>= f = Interval (inf m >>= inf . f) (sup m >>= sup . f)
  {-# INLINE (>>=) #-}

instance MonadTrans Interval where
  lift = point
  {-# INLINE lift #-}

instance (Applicative f, Num a) => Num (Interval f a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  negate = fmap negate
  {-# INLINE negate #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance (Applicative f, Fractional a) => Fractional (Interval f a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance (Applicative f, Floating a) => Floating (Interval f a) where
  pi = pure pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  log = fmap log
  {-# INLINE log #-}
  (**) = liftA2 (**)
  {-# INLINE (**) #-}
  logBase = liftA2 logBase
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  tan = fmap tan
  {-# INLINE tan #-}
  cos = fmap cos
  {-# INLINE cos #-}
  asin = fmap asin
  {-# INLINE asin #-}
  atan = fmap atan
  {-# INLINE atan #-}
  acos = fmap acos
  {-# INLINE acos #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}

instance (Applicative f, Ord a) => Semigroup (Interval f a) where
  (<>) = union
  {-# INLINE (<>) #-}
  stimes = stimesIdempotent
  {-# INLINE stimes #-}


-- Lenses

inf_, sup_ :: Lens' (Interval f a) (f a)

-- | Access the infimum of an interval.
inf_ = lens inf $ \ i inf -> i{ inf }
{-# INLINE inf_ #-}

-- | Access the supremum of an interval.
sup_ = lens sup $ \ i sup -> i{ sup }
{-# INLINE sup_ #-}


-- Constructors

-- | Construct a square interval in @f@ dimensions from the given coordinates.
--
-- >>> import Data.Functor.Identity
-- >>> 0...1 :: Interval Identity Int
-- Identity 0...Identity 1
--
-- >>> import Linear.V2
-- >>> 0...1 :: Interval V2 Int
-- V2 0 0...V2 1 1
(...) :: Applicative f => a -> a -> Interval f a
inf...sup = Interval (pure inf) (pure sup)

infix 3 ...

-- | Construct a point (or /degenerate/) interval from the given endpoint.
--
-- >>> import Linear.V2
-- >>> point (V2 0 1)
-- V2 0 1...V2 0 1
point :: f a -> Interval f a
point p = Interval p p


-- Eliminators

liftI :: Applicative f => (a -> a -> b) -> Interval f a -> f b
liftI f i = liftA2 f (inf i) (sup i)

size :: (Applicative f, Num a) => Interval f a -> f a
size = liftI (flip (-))

toUnit, fromUnit :: (Applicative f, Fractional a) => Interval f a -> f a -> f a
toUnit   i x = liftI (\ inf sup x -> (x - inf) / (sup - inf))        i <*> x
fromUnit i x = liftI (\ inf sup x ->  x        * (sup - inf)  + inf) i <*> x


range :: Enum (f a) => Interval f a -> [f a]
range = enumFromTo . inf <*> sup

ranges :: (Applicative f, Enum a) => Interval f a -> f [a]
ranges = liftI enumFromTo


wrap :: (Applicative f, Real a) => Interval f a -> f a -> f a
wrap i x = liftI (\ inf sup x -> ((x + sup) `mod'` (sup - inf)) + inf) i <*> x


imap :: (f a -> g b) -> Interval f a -> Interval g b
imap f i = Interval (f (inf i)) (f (sup i))


-- Predicates

member :: (Applicative f, Foldable f, Ord a) => f a -> Interval f a -> Bool
member = isSubintervalOf . point


isValid :: (Applicative f, Foldable f, Ord a) => Interval f a -> Bool
isValid i = inf i `lte` sup i

isPoint :: (Applicative f, Foldable f, Eq a) => Interval f a -> Bool
isPoint = and . liftI (==)


-- Relations

isSubintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isSubintervalOf a b = inf a `gte` inf b && sup a `lte` sup b

isSuperintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isSuperintervalOf = flip isSubintervalOf

isProperSubintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isProperSubintervalOf a b = isSubintervalOf a b && or (liftA2 (/=) a b)

isProperSuperintervalOf :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
isProperSuperintervalOf = flip isProperSubintervalOf


before, after :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
before a b = inf a `lte` sup b
after  a b = sup a `lt`  sup b


newtype Union f a = Union { getUnion :: Interval f a }
  deriving (Applicative, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance (Applicative f, Ord a) => Semigroup (Union f a) where
  Union i1 <> Union i2 = Union ((min...max) <*> i1 <*> i2)
  stimes = stimesIdempotent

union :: forall f a . (Applicative f, Ord a) => Interval f a -> Interval f a -> Interval f a
union = coerce ((<>) :: Union f a -> Union f a -> Union f a)


newtype Intersection f a = Intersection { getIntersection :: Interval f a }
  deriving (Applicative, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance (Applicative f, Ord a) => Semigroup (Intersection f a) where
  Intersection i1 <> Intersection i2 = Intersection ((max...min) <*> i1 <*> i2)
  stimes = stimesIdempotent

intersection :: forall f a . (Applicative f, Ord a) => Interval f a -> Interval f a -> Interval f a
intersection = coerce ((<>) :: Intersection f a -> Intersection f a -> Intersection f a)


intersects :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
intersects a b = isValid (intersection a b)


-- Internal

liftRelation :: (Applicative f, Foldable f) => (a -> b -> Bool) -> f a -> f b -> Bool
liftRelation rel a b = and (liftA2 rel a b)

infix 4 `lt`, `lte`, `gte`

lt :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
lt = liftRelation (<)

lte :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
lte = liftRelation (<=)

gte :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
gte = liftRelation (>=)


type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens get put afa s = fmap (put s) (afa (get s))
{-# INLINE lens #-}
