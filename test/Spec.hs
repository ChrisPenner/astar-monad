{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad.AStar
import Test.Hspec hiding (Arg)
import Data.Foldable
import Data.Semigroup
import Control.Monad.Reader
import Control.Lens hiding (Context)

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
            runReader (runAStarT (findN' (3, 6)) ()) (5, 5)
              `shouldBe` Just ([U, R, R], ())
        it "should take the shortest path" $ do
            runReader (debugAStarT (findN' (4, 6)) ()) (5, 5)
              `shouldBe` (Just ([U, R], ()))
        it "should take the shortest path in long situations" $ do
            (length . fst <$> runReader (debugAStarT (findN' (20, 20)) ()) (5, 5))
              `shouldBe` Just 30

    describe "a-star-state" $ do
        it "should find a solution" $ do
            (view moves . snd <$> runAStar findN'' (Context (3, 6) (5, 5) []))
              `shouldBe` Just ([U, R, R])
        it "should take the shortest path" $ do
            (view moves . snd <$> runAStar findN'' (Context (4, 6) (5, 5) []))
              `shouldBe` (Just [U, R])
        it "should take the shortest path in long situations" $ do
            (length . view moves . snd <$> runAStar findN'' (Context (4, 6) (20, 20) []))
              `shouldBe` Just 30

distanceTo :: (Int, Int) -> (Int, Int) -> Int
distanceTo (x, y) (x', y') = abs (x - x') + abs (y - y')

check :: MonadReader (Int, Int) m => (Int, Int) -> m (Either Int (Int, Int))
check coord = do
    goal <- ask
    return $ if coord == goal
                then Right coord
                else Left $ distanceTo goal coord

findN :: (MonadReader (Int, Int) m) => (Int, Int) -> AStarT s (Arg Int (Int, Int)) (Int, Int) m ()
findN (x, y) = do
    measure' check (x, y)
    asum
        [ findN (x + 1, y)
        , findN (x - 1, y)
        , findN (x, y + 1)
        , findN (x, y - 1)
        ]

findN' :: (MonadReader (Int, Int) m) => (Int, Int) -> AStarT s Int [Move] m ()
findN' (x, y) = do
    goal <- lift ask
    if (x, y) == goal
       then done []
       else updateCost $ distanceTo goal (x, y)
    asum
        [ mapResult (R:) $ findN' (x + 1, y)
        , mapResult (L:) $ findN' (x - 1, y)
        , mapResult (D:) $ findN' (x, y + 1)
        , mapResult (U:) $ findN' (x, y - 1)
        ]

findN'' :: Monad m => AStarT Context Int () m ()
findN'' = do
    c <- use current
    gl <- use goal
    if c == gl
       then done ()
       else updateCost $ distanceTo gl c
    asum
        [ moves <>= [R] >> current . _1 += 1 >> findN''
        , moves <>= [L] >> current . _1 -= 1 >> findN''
        , moves <>= [D] >> current . _2 += 1 >> findN''
        , moves <>= [U] >> current . _2 -= 1 >> findN''
        ]
