{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
import Control.Monad.AStar
import Test.Hspec hiding (Arg)
import Data.Foldable
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
    describe "tryWhile" $ do
        it "should stop if weight gets too high" $ do
              -- Use tuple monad to see how far we get
              do flip (tryWhile (< 4)) () $ do
                    asum [ updateCost (10 :: Int) >> lift ([10], ()) >> empty
                          , updateCost (1 :: Int) >> lift ([1], ()) >> empty
                          , updateCost (5 :: Int) >> lift ([5], ()) >> empty
                          , updateCost (3 :: Int) >> lift ([3], ()) >> empty
                         ]
            `shouldBe`
              ([1, 3] :: [Int], Nothing :: Maybe ((), ()))


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
