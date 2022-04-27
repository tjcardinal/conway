module Conway (Coordinate, Point (..), Points, next, surviveXSpawnY) where

import qualified Data.Set as Set

type Coordinate = Int

data Point = Point Coordinate Coordinate
  deriving (Eq, Show, Ord)

type Points = Set.Set Point

next :: (Points -> Point -> Bool) -> Points -> Points
next cond ps =
  Set.filter (cond ps) (pointsWithNeighbors ps)

-- Standard rules are survive 2 spawn 3
surviveXSpawnY :: Int -> Int -> Points -> Point -> Bool
surviveXSpawnY x y ps p =
  (count == x && Set.member p ps) || count == y
  where
    count = Set.size $ Set.intersection ps (neighbors p)

-- Only the already alive points and all adjacent points need to be checked for life
pointsWithNeighbors :: Points -> Points
pointsWithNeighbors ps =
  Set.union ps $ Set.unions $ Set.map neighbors ps

-- Each points 8 neighbors need to be included since they could become alive
neighbors :: Point -> Points
neighbors (Point x y) =
  Set.fromDistinctAscList [Point x' y' | x' <- [x -1 .. x + 1], y' <- [y -1 .. y + 1], x' /= x || y' /= y]