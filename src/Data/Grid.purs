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
  , construct
  , modifyAll
  , modifyAllWithIndex
  ) where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Foldable (class Foldable, foldMap, foldl, foldr, for_)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))

type Coordinates = { x :: Int, y :: Int }
data Grid a = Grid (Array a) { width :: Int, height :: Int }

instance Functor Grid where
  map f (Grid xs cfg) = Grid (map f xs) cfg

instance FunctorWithIndex Coordinates Grid where
  mapWithIndex f g@(Grid _ cfg) = Grid (map (uncurry f) $ enumerate g) cfg

instance Foldable Grid where
  foldr f z (Grid xs _) = foldr f z xs
  foldl f z (Grid xs _) = foldl f z xs
  foldMap f (Grid xs _) = foldMap f xs

enumerate :: forall a. Grid a -> Array (Tuple Coordinates a)
enumerate (Grid xs { width }) = enumerateAsGrid width xs

enumerateAsGrid
  :: forall f a
   . FunctorWithIndex Int f
  => Int
  -> f a
  -> f (Tuple Coordinates a)
enumerateAsGrid width = mapWithIndex go
  where
  go i = Tuple (to2D width i)

to1D :: Int -> Coordinates -> Int
to1D width { x, y } = y * width + x

to2D :: Int -> Int -> Coordinates
to2D width i = { x: i `mod` width, y: i / width }

index :: forall a. Grid a -> Coordinates -> Maybe a
index (Grid xs { height, width }) { x, y }
  | x < 0 || x >= width || y < 0 || y >= height = Nothing
  | otherwise = Array.index xs $ to1D width { x, y }

infixl 8 index as !?

updateAt' :: forall a. Coordinates -> a -> Grid a -> Grid a
updateAt' coords new g = fromMaybe g $ updateAt coords new g

updateAt :: forall a. Coordinates -> a -> Grid a -> Maybe (Grid a)
updateAt coords new = modifyAt coords (const new)

modifyAt' :: forall a. Coordinates -> (a -> a) -> Grid a -> Grid a
modifyAt' coords f g = fromMaybe g $ modifyAt coords f g

modifyAt :: forall a. Coordinates -> (a -> a) -> Grid a -> Maybe (Grid a)
modifyAt coords f (Grid xs cfg@{ width }) =
  map (\ys -> Grid ys cfg) $
    Array.modifyAt (to1D width coords) f xs

modifyAll :: forall a. (a -> a) -> Grid a -> Grid a
modifyAll f = modifyAllWithIndex (const f)

modifyAllWithIndex :: forall a. (Coordinates -> a -> a) -> Grid a -> Grid a
modifyAllWithIndex f (Grid xs cfg) = Grid modified cfg
  where
  modified = STArray.run do
    ys <- STArray.thaw xs
    for_ (enumerateAsGrid cfg.width xs) \(Tuple coords _) ->
      STArray.modify (to1D cfg.width coords) (f coords) ys
    pure ys

replicate :: forall a. Int -> Int -> a -> Grid a
replicate width height x = Grid (Array.replicate (width * height) x) { width, height }

construct :: forall a. Int -> Int -> (Coordinates -> a) -> Grid a
construct width height f = Grid xs { width, height }
  where
  xs = map (f <<< to2D width) $ 0 .. (width * height)

differencesFrom
  :: forall a
   . Eq a
  => Grid a
  -> Grid a
  -> Array (Tuple Coordinates a)
differencesFrom (Grid tiles cfg@{ width }) (Grid tilesReference cfgReference)
  | cfg /= cfgReference = enumerateAsGrid width tiles
  | otherwise = map (\(Tuple i (x /\ _)) -> Tuple i x)
      $ Array.filter (\((Tuple _ (x /\ y))) -> x /= y)
      $ enumerateAsGrid width
      $ (Array.zipWith Tuple tiles tilesReference)
