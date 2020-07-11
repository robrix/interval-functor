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
, diameter
, midpoint
, uncurryI
, liftI
  -- * Enumerations
, enum
, liftEnum
  -- * Conversions
, toUnit
, fromUnit
, lerp
, wrap
  -- * Traversals
, foldMapInterval
, mapInterval
, traverseInterval
  -- * Predicates
, member
, isValid
, isPoint
  -- * Relations
, isSubintervalOf
, isSuperintervalOf
, isProperSubintervalOf
, isProperSuperintervalOf
, intersects
  -- * Semigroups
, Union(..)
, union
, Intersection(..)
, intersection
) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class
import Data.Coerce (coerce)
import Data.Fixed (mod')
import Data.Function (on)
import Data.Semigroup
import GHC.Generics (Generic)

-- | @f@-dimensional intervals with coordinates in @a@.
data Interval f a = Interval
  { inf :: !(f a) -- ^ The infimum, or lower bound.
  , sup :: !(f a) -- ^ The supremum, or upper bound.
  }
  deriving
    ( Eq
    , Foldable    -- ^ Folds over each coordinate of the endpoints. See 'foldMapInterval' for folding over the endpoints themselves.
    , Functor     -- ^ Maps over each coordinate of the endpoints. See 'mapInterval' for mapping over the endpoints themselves.
    , Generic
    , Ord         -- ^ The ordering is defined by @f@, with the infima taking precedence over the suprema.
    , Traversable -- ^ Traverses over each coordinate of the endpoints. See 'traverseInterval' for traversing over the endpoints themselves.
    )

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

-- | '<>' is a synonym for 'union'.
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

-- | Compute the diameter of an interval, defined as the absolute difference between the endpoints.
--
-- Note that the diameter of closed point intervals is zero, so this is not the interval’s /cardinality/.
diameter :: (Applicative f, Num a) => Interval f a -> f a
diameter = liftI (fmap abs . flip (-))

-- | Compute the midpoint of an interval, halfway between the endpoints.
--
-- @
-- midpoint (point x) = x
-- @
midpoint :: (Applicative f, Fractional a) => Interval f a -> f a
midpoint = lerp 0.5

-- | Apply a function to the endpoints of an interval.
--
-- >>> uncurryI (,) (Interval a b)
-- (a, b)
uncurryI :: (f a -> f a -> b) -> Interval f a -> b
uncurryI f i = f (inf i) (sup i)

-- | Lift a function over the coordinates in each dimension of @f@.
--
-- >>> liftI (+) (Interval (V2 1 2) (V2 3 4))
-- V2 4 6
liftI :: Applicative f => (a -> a -> b) -> Interval f a -> f b
liftI f = uncurryI (liftA2 f)


-- Enumerations

-- | Enumerate the points in @f@ between the interval’s endpoints.
--
-- >>> enum (0...1 :: Interval Identity Int)
-- [Identity 0, Identity 1]
enum :: Enum (f a) => Interval f a -> [f a]
enum = uncurryI enumFromTo

-- | Enumerate the coordinates in @a@ between the interval’s endpoints along each dimension of @f@.
--
-- >>> liftEnum (Interval (V2 1 2) (V2 1 3))
-- V2 [1] [2, 3]
liftEnum :: (Applicative f, Enum a) => Interval f a -> f [a]
liftEnum = liftI enumFromTo


-- Conversions

toUnit, fromUnit :: (Applicative f, Fractional a) => Interval f a -> f a -> f a

-- | Linearly transform a point in @f@ from an interval of @f@ to the unit interval.
toUnit   i x = liftI (\ inf sup x -> (x - inf) / (sup - inf))        i <*> x

-- | Linearly transform a point in @f@ from the unit interval to an interval of @f@.
fromUnit i x = liftI (\ inf sup x ->  x        * (sup - inf)  + inf) i <*> x

-- | Linearly interpolate between the endpoints of an interval.
--
-- @
-- lerp 0 = inf
-- @
-- @
-- lerp 1 = sup
-- @
lerp :: (Applicative f, Num a) => a -> Interval f a -> f a
lerp t = liftI (\ inf sup -> (1 - t) * inf + t * sup)

-- | Clamp a point in @f@ to the given interval, wrapping out-of-bounds values around.
--
-- e.g. to wrap angles in radians to the interval [-pi, pi]:
--
-- >>> wrap (-pi...pi) (pi + x)
-- Identity (-pi + x)
wrap :: (Applicative f, Real a) => Interval f a -> f a -> f a
wrap i x = liftI (\ inf sup x -> ((x + sup) `mod'` (sup - inf)) + inf) i <*> x


-- Traversals

-- | Map and fold over an interval’s endpoints.
--
-- Where 'foldMap' only folds over the individual coordinates, 'foldMapInterval' can interpret the structure of the space as well.
--
-- >>> foldMapInterval (\ p -> [p]) (Interval a b)
-- [a, b]
--
-- @
-- foldMap f = foldMapInterval (foldMap f)
-- @
foldMapInterval :: Semigroup s => (f a -> s) -> Interval f a -> s
foldMapInterval f = uncurryI ((<>) `on` f)

-- | Map over an interval’s endpoints.
--
-- Where 'fmap' only maps over the individual coordinates, 'mapInterval' can change the space as well.
--
-- >>> mapInterval (\ (V2 x y) -> V3 x y 0) (Interval (V2 1 2) (V2 3 4))
-- V3 1 2 0...V3 3 4 0
--
-- @
-- fmap f = mapInterval (fmap f)
-- @
mapInterval :: (f a -> g b) -> Interval f a -> Interval g b
mapInterval f = uncurryI (Interval `on` f)

-- | Traverse over an interval’s endpoints.
--
-- Where 'traverse' only traverses over the individual coordinates, 'traverseInterval' can change the space as well.
--
-- >>> :t traverseInterval (\ (V2 x y) -> V3 x y)
-- traverseInterval (\ (V2 x y) -> V3 x y) :: Interval V2 a -> a -> Interval V3 a
--
-- >>> traverseInterval (\ (V2 x y) -> V3 x y) (Interval (V2 1 2) (V2 3 4)) 0
-- V3 1 2 0...V3 3 4 0
--
-- @
-- traverse f = traverseInterval (traverse f)
-- @
-- @
-- foldMapInterval f ≅ getConst . traverseInterval (Const . f)
-- @
-- @
-- mapInterval f = runIdentity . traverseInterval (Identity . f)
-- @
traverseInterval :: Applicative m => (f a -> m (g b)) -> Interval f a -> m (Interval g b)
traverseInterval f = uncurryI (liftA2 Interval `on` f)


-- Predicates

-- | Test a point for inclusion within an interval.
member :: (Applicative f, Foldable f, Ord a) => f a -> Interval f a -> Bool
member = isSubintervalOf . point


-- | Test an interval for validity, i.e. non-emptiness.
isValid :: (Applicative f, Foldable f, Ord a) => Interval f a -> Bool
isValid = uncurryI lte

-- | Test whether an interval is a singleton.
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


intersects :: (Applicative f, Foldable f, Ord a) => Interval f a -> Interval f a -> Bool
intersects a b = isValid (intersection a b)


-- Semigroups

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


-- Internal

liftRelation :: (Applicative f, Foldable f) => (a -> b -> Bool) -> f a -> f b -> Bool
liftRelation rel a b = and (liftA2 rel a b)

infix 4 `lte`, `gte`

lte, gte :: (Applicative f, Foldable f, Ord a) => f a -> f a -> Bool
lte = liftRelation (<=)
gte = liftRelation (>=)


type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens get put afa s = fmap (put s) (afa (get s))
{-# INLINE lens #-}
