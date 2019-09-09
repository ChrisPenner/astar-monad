{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.AStar
import Test.Hspec hiding (Arg)
import Data.Foldable
import Data.Semigroup
import Control.Monad.Reader

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
            runReader (runAStarT (findN (0, 0))) (5, 5)
              `shouldBe` Just (5, 5)
        it "should take the shortest path" $ do
            runReader (debugAStarT (findN (4, 6))) (5, 5)
              `shouldBe` ([(Arg 2 (4, 6)), Arg 1 (4, 5)], Just (5, 5))
        it "should take the shortest path in long situations" $ do
            (length . fst $ runReader (debugAStarT $ findN (20, 20)) (5, 5))
              `shouldBe` 30

distanceTo :: (Int, Int) -> (Int, Int) -> Int
distanceTo (x, y) (x', y') = ((x - x')^2 + (y - y')^2)

check :: MonadReader (Int, Int) m => (Int, Int) -> m (Either Int (Int, Int))
check coord = do
    goal <- ask
    return $ if coord == goal
                then Right coord
                else Left $ distanceTo goal coord

findN :: (MonadReader (Int, Int) m) => (Int, Int) -> AStarT (Arg Int (Int, Int)) (Int, Int) m ()
findN (x, y) = do
    measure' check (x, y)
    asum
        [ findN (x + 1, y)
        , findN (x - 1, y)
        , findN (x, y + 1)
        , findN (x, y - 1)
        ]
