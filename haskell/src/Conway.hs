module Conway (Coordinate, Point (..), Points, next, surviveXSpawnY) where

import qualified Data.Set as Set

type Coordinate = Int

data Point = Point Coordinate Coordinate
  deriving (Eq, Show, Ord)

type Points = Set.Set Point

next :: (Points -> Point -> Bool) -> Points -> Points
next cond ps =
  Set.filter (cond ps) (pointsWithNeighbors ps)

surviveXSpawnY :: Int -> Int -> Points -> Point -> Bool
surviveXSpawnY x y ps p =
  (count == x && Set.member p ps) || count == y
  where
    count = Set.size $ Set.intersection ps (neighbors p)

pointsWithNeighbors :: Points -> Points
pointsWithNeighbors ps =
  Set.union ps $ Set.unions $ Set.map neighbors ps

neighbors :: Point -> Points
neighbors (Point x y) =
  Set.fromDistinctAscList [Point x' y' | x' <- [x -1 .. x + 1], y' <- [y -1 .. y + 1], x' /= x || y' /= y]