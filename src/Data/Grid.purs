-- | This module supplies a simple datastructure for working with 2D grids.
-- | `Grid` is based on (immutable) arrays from the module `Data.Array`, so it has
-- | similar performance characteristics: fast access (anywhere), slow updates
-- | (the whole structure is copied), efficient memory usage.

module Data.Grid
  ( Grid(..)
  , enumerate
  , differencesFrom
  , updateAt
  , modifyAt
  , updateAt'
  , modifyAt'
  , index
  , replicate
  , Coordinates
  , fromFoldable
  , construct
  ) where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr, length)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))

-- | A helper type representing a point on a grid.
type Coordinates = { x :: Int, y :: Int }

-- | A grid is represented a plain 1D array. It is saved there row by row, starting from the top.
-- | Functions in this module take 2D coordinates, but translate them interally to 1D index
-- | into the array.
data Grid a = Grid (Array a) { width :: Int, height :: Int }

instance Functor Grid where
  map f (Grid xs cfg) = Grid (map f xs) cfg

instance FunctorWithIndex Coordinates Grid where
  mapWithIndex f (Grid xs cfg) = Grid (Array.mapWithIndex (f <<< to2D cfg.width) xs) cfg

instance Foldable Grid where
  foldr f z (Grid xs _) = foldr f z xs
  foldl f z (Grid xs _) = foldl f z xs
  foldMap f (Grid xs _) = foldMap f xs

-- | Create a `Grid` of the given width and height from any `Foldable`, like arrays and lists.
-- | If the number of elements in array isn't exactly equal to `width * height` — in other words, there are
-- | too many or too few elements — return `Nothing`.&.
fromFoldable :: forall f a. Foldable f ⇒ Int -> Int -> f a → Maybe (Grid a)
fromFoldable width height xs
  | length xs /= width * height = Nothing
  | otherwise = Just $ Grid (Array.fromFoldable xs) { width, height }

-- | Return the elements of the grid in a flat array, within a tuple containing their original 2D coordinates.
enumerate :: forall a. Grid a -> Array (Tuple Coordinates a)
enumerate g = xs
  where
  Grid xs _ = mapWithIndex Tuple g

to1D :: Int -> Coordinates -> Int
to1D width { x, y } = y * width + x

to2D :: Int -> Int -> Coordinates
to2D width i = { x: i `mod` width, y: i / width }

-- | This function provides a safe way to read a value at a particular index from a grid.
-- | The position `{ x: 0, y: 0 }` is in the top left corner of the grid.
-- | ```
-- | sentence = Grid.fromFoldable 2 2 [ "Hello", "World", "Guys", "!" ]
-- |
-- | index sentence { x:0, y:0 } = Just "Hello"
-- | index sentence { x:2, y:0 } = Nothing
-- | ```
index :: forall a. Grid a -> Coordinates -> Maybe a
index (Grid xs { height, width }) { x, y }
  | x < 0 || x >= width || y < 0 || y >= height = Nothing
  | otherwise = Array.index xs $ to1D width { x, y }

-- | Operator alias for `index`.
-- | ```
-- | index sentence { x:1, y:0 } = sentence !? { x:1, y:0 }
-- | ```
infixl 8 index as !?

-- | Change the element at the specified index, creating a new grid,
-- | or don't do anything  when the index is out of bounds.
updateAt' :: forall a. Coordinates -> a -> Grid a -> Grid a
updateAt' coords new g = fromMaybe g $ updateAt coords new g

-- | Change the element at the specified index, creating a new grid, or returning `Nothing` if the index is out of bounds.
updateAt :: forall a. Coordinates -> a -> Grid a -> Maybe (Grid a)
updateAt coords new = modifyAt coords (const new)

-- | Change the element at the specified index, creating a new grid, or don't do anything  when the index is out of bounds.
modifyAt' :: forall a. Coordinates -> (a -> a) -> Grid a -> Grid a
modifyAt' coords f g = fromMaybe g $ modifyAt coords f g

-- | Apply a function to the element at the specified index, creating a new grid,
-- | or returning `Nothing` if the index is out of bounds.
modifyAt :: forall a. Coordinates -> (a -> a) -> Grid a -> Maybe (Grid a)
modifyAt coords f (Grid xs cfg@{ width }) =
  map (\ys -> Grid ys cfg) $
    Array.modifyAt (to1D width coords) f xs

-- | Fill a grid of size `width x height` with the value `a`. The value is the same
-- | in all of the cells in the grid.
replicate :: forall a. Int -> Int -> a -> Grid a
replicate width height x = Grid (Array.replicate (width * height) x) { width, height }

-- | Create a grid of size `width x height` filled with values returned by a `(Coordinates -> a)` function.
-- | The funcion gets called on every coordinate in the grid and the value it returns is used to fill that specific position.
-- | The smallest coordinate is `{ x: 0, y: 0 }` in the upper-left corner of the grid.
-- |
-- | It is possible to implement `replicate` with the help of `construct`:
-- | ```
-- | replicate w h value = Grid.construct w h (\_ -> value)
-- | ```
-- |
-- | For example, `Grid.construct 3 3 (\{x, y} -> (x - y) == 0)` would create the following grid
-- | ```
-- | true  false false
-- | false true  false
-- | false false true
-- | ```
construct :: forall a. Int -> Int -> (Coordinates -> a) -> Grid a
construct width height f = Grid xs { width, height }
  where
  xs = map (f <<< to2D width) $ 0 .. (width * height)

-- | Return an array of elements and their coordinates where the two supplied grids differ.
-- | If the grids have different dimensions, return all of the elements from the first grid.
differencesFrom
  :: forall a
   . Eq a
  => Grid a
  -> Grid a
  -> Array (Tuple Coordinates a)
differencesFrom g@(Grid tiles cfg@{ width }) (Grid tilesReference cfgReference)
  | cfg /= cfgReference = enumerate g
  | otherwise = map (\(Tuple i (x /\ _)) -> Tuple (to2D width i) x)
      $ Array.filter (\((Tuple _ (x /\ y))) -> x /= y)
      $ Array.mapWithIndex Tuple
      $ Array.zipWith Tuple tiles tilesReference
