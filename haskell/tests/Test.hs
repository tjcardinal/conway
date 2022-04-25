import Test.Tasty
import Test.Tasty.HUnit
import Conway
import Data.Set as Set

main = defaultMain tests

tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit Tests"
  [ let ps = Set.fromDistinctAscList [Point 1 1, Point 1 2, Point 2 1, Point 2 2]
    in testCase "Block" $ ps  @=? next (surviveXSpawnY 2 3) ps,

    let ps = Set.fromDistinctAscList [Point 1 2, Point 2 1, Point 2 3, Point 3 2]
    in testCase "Tub" $ ps  @=? next (surviveXSpawnY 2 3) ps,

    let ps = Set.fromDistinctAscList [Point 2 1, Point 2 2, Point 2 3]
        ps' = Set.fromDistinctAscList [Point 1 2, Point 2 2, Point 3 2]
    in testCase "Blinker" $ ps'  @=? next (surviveXSpawnY 2 3) ps,

    let ps = Set.fromDistinctAscList [Point 2 2, Point 2 3, Point 2 4, Point 3 1, Point 3 2, Point 3 3]
        ps' = Set.fromDistinctAscList [Point 1 3, Point 2 1, Point 2 4, Point 3 1, Point 3 4, Point 4 2]
    in testCase "Toad" $ ps'  @=? next (surviveXSpawnY 2 3) ps
  ]