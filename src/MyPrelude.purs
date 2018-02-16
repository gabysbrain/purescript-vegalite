module MyPrelude 
 ( module Control.Applicative
  , module Control.Apply
  , module Control.Bind
  , module Control.Category
  , module Control.Monad
  , module Control.Monad.Reader
  , module Control.Semigroupoid
  , module Data.Boolean
  , module Data.BooleanAlgebra
  , module Data.Bounded
  , module Data.CommutativeRing
  , module Data.Either
  , module Data.Eq
  , module Data.EuclideanRing
  , module Data.Field
  , module Data.Foldable
  , module Data.Function
  , module Data.Functor
  , module Data.HeytingAlgebra
  , module Data.List
  , module Data.Maybe
  , module Data.Monoid
  , module Data.NaturalTransformation
  , module Data.Newtype
  , module Data.Ord
  , module Data.Ordering
  , module Data.Ring
  , module Data.Semigroup
  , module Data.Semiring
  , module Data.Show
  , module Data.Set
  , module Data.Tuple
  , module Data.Unit
  , module Data.Void
  ) where

import Control.Applicative (class Applicative, pure, liftA1, unless, when)
import Control.Apply (class Apply, apply, (*>), (<*), (<*>))
import Control.Bind (class Bind, bind, class Discard, discard, ifM, join, (<=<), (=<<), (>=>), (>>=))
import Control.Category (class Category, id)
import Control.Monad (class Monad, ap, liftM1, unlessM, whenM)
import Control.Monad.Reader (Reader, ask, runReader, withReader, mapReader)
import Control.Semigroupoid (class Semigroupoid, compose, (<<<), (>>>))

import Data.Boolean (otherwise)
import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.Bounded (class Bounded, bottom, top)
import Data.CommutativeRing (class CommutativeRing)
import Data.Either (Either, either)
import Data.Eq (class Eq, eq, notEq, (/=), (==))
import Data.EuclideanRing (class EuclideanRing, degree, mod, (/), gcd, lcm)
import Data.Field (class Field)
import Data.Foldable (class Foldable, foldl, foldr, foldMap, for_)
import Data.Function (const, flip, ($), (#))
import Data.Functor (class Functor, flap, map, void, ($>), (<#>), (<$), (<$>), (<@>))
import Data.HeytingAlgebra (class HeytingAlgebra, conj, disj, not, (&&), (||))
import Data.List (List(Cons, Nil), (:))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.NaturalTransformation (type (~>))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Ord (class Ord, compare, (<), (<=), (>), (>=), comparing, min, max, clamp, between)
import Data.Ordering (Ordering(..))
import Data.Ring (class Ring, negate, sub, (-))
import Data.Semigroup (class Semigroup, append, (<>))
import Data.Semiring (class Semiring, one, zero, (*), (+))
import Data.Set (Set)
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)

