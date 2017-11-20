module Data.Map.Justified
  ( Key
  , Map
  , empty
  , lookup
  , member
  , run
  , showTree
  , singleton
  , unJustified
  , unKeyed
  , with
  ) where

import Data.Boolean (otherwise)
import Data.Eq (class Eq, class Eq1)
import Data.Foldable (class Foldable)
import Data.Function (($))
import Data.Functor (class Functor)
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid (class Monoid)
import Data.Ord (class Ord, class Ord1)
import Data.Semigroup (class Semigroup)
import Data.Show (class Show)
import Data.Traversable (class Traversable)
import Partial.Unsafe (unsafePartial)

newtype Map proof k v
  = Map (M.Map k v)

derive newtype instance eq1Map :: (Eq k) => Eq1 (Map proof k)
derive newtype instance eqMap :: (Eq k, Eq v) => Eq (Map proof k v)
derive newtype instance foldableMap :: Foldable (Map proof k)
derive newtype instance functorMap :: Functor (Map proof k)
derive newtype instance monoidMap :: (Ord k) => Monoid (Map proof k v)
derive newtype instance ord1Map :: (Ord k) => Ord1 (Map proof k)
derive newtype instance ordMap :: (Ord k, Ord v) => Ord (Map proof k v)
derive newtype instance semigroupMap :: (Ord k) => Semigroup (Map proof k v)
derive newtype instance showMap :: (Show k, Show v) => Show (Map proof k v)
derive newtype instance traversableMap :: Traversable (Map proof k)

unJustified :: forall k proof v. Map proof k v -> M.Map k v
unJustified (Map m) = m

run :: forall a k v. (forall proof. Map proof k v -> a) -> M.Map k v -> a
run f m = f (Map m)

with :: forall a k v. M.Map k v -> (forall proof. Map proof k v -> a) -> a
with m f = f (Map m)

newtype Key proof k
  = Key k

unKeyed :: forall k proof. Key proof k -> k
unKeyed (Key k) = k

member :: forall k proof v. Ord k => k -> Map proof k v -> Maybe (Key proof k)
member k (Map m)
  | M.member k m = Just $ Key k
  | otherwise = Nothing

showTree :: forall k proof v. Show k => Show v => Map proof k v -> String
showTree (Map m) = M.showTree m

empty :: forall k proof v. Map proof k v
empty = Map M.empty

singleton :: forall k proof v. k -> v -> Map proof k v
singleton k v = Map $ M.singleton k v

lookup :: forall k proof v. Ord k => Key proof k -> Map proof k v -> v
lookup (Key k) (Map m) = unsafePartial $ fromJust $ M.lookup k m
