{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Control.Monad.AStar
import Test.Hspec hiding (Arg, context)
import Data.Foldable
import Control.Lens hiding (Context)
import Control.Monad.State
import Control.Applicative
import Data.Monoid

data Move = U | D | L | R
    deriving (Show, Eq)

data Context =
    Context { _current   :: (Int, Int)
            , _goal      :: (Int, Int)
            , _moves     :: [Move]
            , _locations :: [(Int, Int)]
            }
    deriving (Show, Eq)

makeLenses ''Context

context :: (Int, Int) -> (Int, Int) -> Context
context start goal = Context start goal [] []

main :: IO ()
main = hspec $ do
    describe "a-star" $ do
        it "should find a solution" $ do
            (fst <$> runAStar findPoint (context (3, 6) (5, 5)))
              `shouldBe` Just (5, 5)
        it "should take the shortest path" $ do
            (view moves . snd <$> runAStar findPoint (context (4, 6) (5, 5)))
              `shouldBe` Just [R, U]
        it "should take the shortest path in long situations" $ do
            (length . view moves . snd <$> runAStar findPoint (context (4, 6) (20, 20)))
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
    --           do flip (tryWhileT (const (< 4))) () $ do
    --                 asum [ estimate (10 :: Sum Int) >> lift ([10], ()) >> empty
    --                       , estimate (1 :: Sum Int) >> lift ([1], ()) >> empty
    --                       , estimate (5 :: Sum Int) >> lift ([5], ()) >> empty
    --                       , estimate (3 :: Sum Int) >> lift ([3], ()) >> empty
    --                      ]
    --         `shouldBe`
    --           ([1, 3] :: [Int], Nothing :: Maybe ((), ()))


distanceTo :: (Int, Int) -> (Int, Int) -> Int
distanceTo (x, y) (x', y') = abs (x - x') + abs (y - y')

move :: Monad m => Move -> AStarT Context (Sum Int) r m ()
move L = moves <>= [L] >> current . _1 -= 1
move R = moves <>= [R] >> current . _1 += 1
move U = moves <>= [U] >> current . _2 -= 1
move D = moves <>= [D] >> current . _2 += 1

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

-- To make things more interesting we'll say that moving through coordinates
-- where either 'x' or 'y' are divisible by the other is 3X more expensive!
addCost :: MonadAStar (Sum Int) r m => (Int, Int) -> m ()
-- Don't divide by zero!
addCost (0, _) = spend 1
addCost (_, 0) = spend 1
addCost (x, y)
    | mod x y == 0 || mod y x == 0 = spend 3
addCost _ = spend 1


mineField :: AStar Context (Sum Int) (Int, Int) ()
mineField = do
    -- Get the current position
    (x, y) <- use current
    -- Add the current location to our list
    locations <>= [(x, y)]
    -- If we're at the goal we're done!
    gl <- use goal
    when ((x, y) == gl)
      $ done gl
    -- Add the cost of the current position
    addCost (x, y)
    -- Estimate steps to completion
    estimate . Sum $ distanceTo gl (x, y)
    asum
        [ move L >> mineField
        , move R >> mineField
        , move U >> mineField
        , move D >> mineField
        ]
