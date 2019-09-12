{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Control.Monad.AStar
import Test.Hspec hiding (Arg)
import Data.Foldable
import Control.Lens hiding (Context)
import Control.Monad.State
import Control.Applicative
import Data.Monoid

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
            (fst <$> runAStar findPoint (Context (3, 6) (5, 5) []))
              `shouldBe` Just (5, 5)
        it "should take the shortest path" $ do
            (view moves . snd <$> runAStar findPoint (Context (4, 6) (5, 5) []))
              `shouldBe` Just [R, U]
        it "should take the shortest path in long situations" $ do
            (length . view moves . snd <$> runAStar findPoint (Context (4, 6) (20, 20) []))
              `shouldBe` Just 30
        -- it "should properly rewind state" $ do
        --       do flip runAStar [] $ do
        --             asum [ estimate (1 :: Int) >> modify (++ [1]) >> estimate 10 >> modify (++ [10])
        --                  , estimate (2 :: Int) >> modify (++ [2]) >> done ()
        --                  ]
        it "should resolve with Nothing if branches return after updating cost" $ do
              do flip runAStar () $ (estimate @(Sum Int) 1 >> return ()) <|> return ()
            `shouldBe`
              (Nothing :: Maybe ((), ()))
        it "should resolve with solution if some branches simply return" $ do
              do flip runAStar () $ (return () <|> (estimate @(Sum Int) 1 >> done ()))
            `shouldBe`
              Just ((), ())
        it "should resolve without solution if all branches simply return" $ do
              do flip runAStar () $ (return () <|> return () :: AStar () () () ())
            `shouldBe`
              (Nothing :: Maybe ((), ()))
    -- describe "tryWhile" $ do
    --     it "should stop if weight gets too high" $ do
    --           -- Use tuple monad to see how far we get
    --           do flip (tryWhileT (< 4)) () $ do
    --                 asum [ estimate (10 :: Int) >> lift ([10], ()) >> empty
    --                       , estimate (1 :: Int) >> lift ([1], ()) >> empty
    --                       , estimate (5 :: Int) >> lift ([5], ()) >> empty
    --                       , estimate (3 :: Int) >> lift ([3], ()) >> empty
    --                      ]
    --         `shouldBe`
    --           ([1, 3] :: [Int], Nothing :: Maybe ((), ()))


distanceTo :: (Int, Int) -> (Int, Int) -> Int
distanceTo (x, y) (x', y') = abs (x - x') + abs (y - y')

move :: Monad m => Move -> AStarT Context (Sum Int) r m ()
move L = moves <>= [L] >> current . _1 -= 1 >> spend 1
move R = moves <>= [R] >> current . _1 += 1 >> spend 1
move U = moves <>= [U] >> current . _2 -= 1 >> spend 1
move D = moves <>= [D] >> current . _2 += 1 >> spend 1

findPoint :: AStar Context (Sum Int) (Int, Int) ()
findPoint = do
    c <- use current
    gl <- use goal
    when (c == gl) $ done gl
    estimate . Sum $ distanceTo gl c
    asum
        [ move L >> findPoint
        , move R >> findPoint
        , move U >> findPoint
        , move D >> findPoint
        ]
