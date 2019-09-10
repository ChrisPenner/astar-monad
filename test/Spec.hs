{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
import Control.Monad.AStar
import Test.Hspec hiding (Arg)
import Data.Foldable
import Data.Semigroup
import Control.Lens hiding (Context)
import Control.Monad.State
import Control.Applicative

data Move = U | D | L | R
    deriving (Show, Eq)

data Context =
    Context { _current :: (Int, Int)
            , _goal    :: (Int, Int)
            , _moves   :: [Move]
            }
    deriving (Show, Eq)

makeLenses ''Context

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
            (view moves . snd <$> runAStar findN (Context (3, 6) (5, 5) []))
              `shouldBe` Just ([U, R, R])
        it "should take the shortest path" $ do
            (view moves . snd <$> runAStar findN (Context (4, 6) (5, 5) []))
              `shouldBe` (Just [U, R])
        it "should take the shortest path in long situations" $ do
            (length . view moves . snd <$> runAStar findN (Context (4, 6) (20, 20) []))
              `shouldBe` Just 30
    describe "a-star" $ do
        it "should stop if weight gets too high" $ do
            shouldBe
              do fst . flip runAStarT () . tryWhile (< 2) $ do
                    asum [ updateCost (10 :: Int) >> lift ([10], ()) >> empty
                          , updateCost (1 :: Int) >> lift ([1], ()) >> empty
                          , updateCost (5 :: Int) >> lift ([5], ()) >> done ()
                          , updateCost (3 :: Int) >> lift ([3], ()) >> empty
                         ]
              ([1, 2] :: [Int])

distanceTo :: (Int, Int) -> (Int, Int) -> Int
distanceTo (x, y) (x', y') = abs (x - x') + abs (y - y')

findN :: Monad m => AStarT Context Int () m ()
findN = do
    c <- use current
    gl <- use goal
    if c == gl
       then done ()
       else updateCost $ distanceTo gl c
    asum
        [ moves <>= [R] >> current . _1 += 1 >> findN
        , moves <>= [L] >> current . _1 -= 1 >> findN
        , moves <>= [D] >> current . _2 += 1 >> findN
        , moves <>= [U] >> current . _2 -= 1 >> findN
        ]
