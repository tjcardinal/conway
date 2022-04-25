module Main where

import Conway
import qualified Data.Set as Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

pixelSize = 10

toad :: Points
toad =
  Set.fromDistinctAscList
    [ Point 1 2,
      Point 1 3,
      Point 1 4,
      Point 2 1,
      Point 2 2,
      Point 2 3
    ]

window = InWindow "Conway" (200, 200) (10, 10)

pointsToPicture :: Points -> Picture
pointsToPicture ps =
  Pictures $ map f (Set.elems ps)
  where
    f = \(Point x y) -> Translate (pixelSize * fromIntegral x) (pixelSize * fromIntegral y) (rectangleSolid pixelSize pixelSize)

updateModel :: ViewPort -> Float -> Points -> Points
updateModel _ _ =
  next (surviveXSpawnY 2 3)

main :: IO ()
main = do
  simulate window white 1 toad pointsToPicture updateModel