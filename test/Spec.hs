{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
import Control.Monad.AStar
import Test.Hspec hiding (Arg)
import Data.Foldable
import Data.Semigroup

argFirst :: Arg a b -> a
argFirst (Arg a _) = a

argSecond :: Arg a b -> b
argSecond (Arg _ b) = b


mapWeight :: (w -> x) -> Step w r a -> Step x r a
mapWeight f (Weighted w) = Weighted $ f w
mapWeight _ (Solved r) = Solved r
mapWeight _ (Pure a) = Pure a


main :: IO ()
main = hspec $ do
    describe "a-star" $ do
        it "should find a solution" $ do
            runAStar (distanceTo (5, 5)) (findN (0, 0))
              `shouldBe` Just (5, 5)
        it "should take the shortest path" $ do
            debugAStar (distanceTo (5, 5)) (findN (4, 6))
              `shouldBe` ([(2, (4, 6)), (1, (4, 5))], Just (5, 5))
        it "should take the shortest path in long situations" $ do
            (length . fst $ debugAStar (distanceTo (5, 5)) (findN (20, 20)))
              `shouldBe` 30


distanceTo :: (Int, Int) -> (Int, Int) -> Maybe Int
distanceTo a b | a == b = Nothing
distanceTo (x, y) (x', y') = Just $ ((x - x')^2 + (y - y')^2)

findN :: (Int, Int) -> AStar (Int, Int) ()
findN (x, y) = do
    measure (x, y)
    asum
        [ findN (x + 1, y)
        , findN (x - 1, y)
        , findN (x, y + 1)
        , findN (x, y - 1)
        ]
