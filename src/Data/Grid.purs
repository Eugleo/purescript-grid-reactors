module Data.Grid
  ( Grid(..)
  , enumerate
  , mapWithIndex
  , differencesFrom
  , updateAt
  , modifyAt
  , index
  , replicate
  , construct
  ) where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))

data Grid a = Grid (Array a) { width :: Int, height :: Int }

instance Functor Grid where
  map f (Grid xs cfg) = Grid (map f xs) cfg

mapWithIndex :: forall a b. Grid a -> ({ x :: Int, y :: Int } -> a -> b) -> Grid b
mapWithIndex g@(Grid _ cfg) f = Grid (map (uncurry f) $ enumerate g) cfg

enumerate :: forall a. Grid a -> Array (Tuple { x :: Int, y :: Int } a)
enumerate (Grid xs { width }) = enumerate2D width xs

enumerate2D :: forall a. Int -> Array a -> Array (Tuple { x :: Int, y :: Int } a)
enumerate2D width = Array.mapWithIndex go
  where
  go i = Tuple (to2D width i)

to1D :: Int -> Int -> Int -> Int
to1D width x y = y * width + x

to2D :: Int -> Int -> { x :: Int, y :: Int }
to2D width i = { x: i `mod` width, y: i / width }

index :: forall a. Grid a -> Int -> Int -> Maybe a
index (Grid xs { width }) x y = Array.index xs $ to1D width x y

infixl 8 index as !?

updateAt :: forall a. Int -> Int -> a -> Grid a -> Maybe (Grid a)
updateAt x y new (Grid xs cfg@{ width }) =
  map (\ys -> Grid ys cfg) $
    Array.updateAt (to1D width x y) new xs

modifyAt :: forall a. Int -> Int -> (a -> a) -> Grid a -> Maybe (Grid a)
modifyAt x y f (Grid xs cfg@{ width }) =
  map (\ys -> Grid ys cfg) $
    Array.modifyAt (to1D width x y) f xs

replicate :: forall a. Int -> Int -> a -> Grid a
replicate width height x = Grid (Array.replicate (width * height) x) { width, height }

construct :: forall a. Int -> Int -> ({ x :: Int, y :: Int } -> a) -> Grid a
construct width height f = Grid xs { width, height }
  where
  xs = map (f <<< to2D width) $ 0 .. (width * height)

differencesFrom
  :: forall a
   . Eq a
  => Grid a
  -> Grid a
  -> Array (Tuple { x :: Int, y :: Int } a)
differencesFrom (Grid tiles cfg@{ width }) (Grid tilesReference cfgReference)
  | cfg /= cfgReference = enumerate2D width tiles
  | otherwise = map (\(Tuple i (x /\ _)) -> Tuple i x)
      $ Array.filter (\((Tuple _ (x /\ y))) -> x /= y)
      $ enumerate2D width
      $ (Array.zipWith Tuple tiles tilesReference)
