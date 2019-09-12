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
            (join . fmap result $ runAStar findPoint (Context (3, 6) (5, 5) []))
              `shouldBe` Just (5, 5)
        it "should take the shortest path" $ do
            (view moves . branchState <$> runAStar findPoint (Context (4, 6) (5, 5) []))
              `shouldBe` Just [U, R]
        it "should take the shortest path in long situations" $ do
            (length . view moves . branchState <$> runAStar findPoint (Context (4, 6) (20, 20) []))
              `shouldBe` Just 30
        -- it "should properly rewind state" $ do
        --       do flip execAStar [] $ do
        --             asum [ updateCost (1 :: Int) >> modify (++ [1]) >> updateCost 10 >> modify (++ [10])
        --                  , updateCost (2 :: Int) >> modify (++ [2]) >> done ()
        --                  ]
        it "should resolve with Nothing if branches return after updating cost" $ do
              do flip runAStar () $ (updateCost 1 >> return ()) <|> return ()
            `shouldBe`
              (Nothing :: Maybe (BranchState () (Sum Int) ()))
        it "should resolve with solution if some branches simply return" $ do
              do flip runAStar () $ (return () <|> (updateCost 1 >> done ()))
            `shouldBe`
              Just (BranchState () (Sum 1) (Just ()))
        it "should resolve without solution if all branches simply return" $ do
              do flip runAStar () $ (return () <|> return () :: AStar () () () ())
            `shouldBe`
              (Nothing :: Maybe (BranchState () () ()))
    -- describe "tryWhile" $ do
    --     it "should stop if weight gets too high" $ do
    --           -- Use tuple monad to see how far we get
    --           do flip (tryWhileT (< 4)) () $ do
    --                 asum [ updateCost (10 :: Int) >> lift ([10], ()) >> empty
    --                       , updateCost (1 :: Int) >> lift ([1], ()) >> empty
    --                       , updateCost (5 :: Int) >> lift ([5], ()) >> empty
    --                       , updateCost (3 :: Int) >> lift ([3], ()) >> empty
    --                      ]
    --         `shouldBe`
    --           ([1, 3] :: [Int], Nothing :: Maybe ((), ()))


distanceTo :: (Int, Int) -> (Int, Int) -> Int
distanceTo (x, y) (x', y') = abs (x - x') + abs (y - y')

findPoint :: AStar Context (Sum Int) (Int, Int) ()
findPoint = do
    c <- use current
    gl <- use goal
    when (c == gl) $ done gl
    updateCost . Sum $ distanceTo gl c
    asum
        [ moves <>= [R] >> current . _1 += 1 >> findPoint
        , moves <>= [L] >> current . _1 -= 1 >> findPoint
        , moves <>= [D] >> current . _2 += 1 >> findPoint
        , moves <>= [U] >> current . _2 -= 1 >> findPoint
        ]
