{-# LANGUAGE RankNTypes #-}
module Control.Monad.AStar where

import Control.Monad.Logic
import Control.Applicative
import Data.Maybe
import Data.Foldable


data Step w a = Solved a | Continue w
    deriving Show

weightedInterleave :: (Ord w, MonadLogic m) => m (Step w a) -> m (Step w a) -> m (Step w a)
weightedInterleave a b = do
    rA <- msplit a
    rB <- msplit b
    case (rA, rB) of
        (m, Nothing) -> reflect m
        (Nothing, m) -> reflect m
        (Just (Solved a, _), _) -> return (Solved a)
        (_ , Just (Solved a, _)) -> return (Solved a)
        (l@(Just (Continue lw, lm)), r@(Just (Continue rw, rm))) ->
            if lw < rw
               then reflect (Just (Continue lw, weightedInterleave lm (reflect r)))
               else reflect (Just (Continue rw, weightedInterleave rm (reflect l)))

solve :: MonadLogic m => m (Step w a) -> m a
solve m = do
    r <- msplit m
    case r of
        Nothing -> empty
        Just (Continue _, m) -> solve m
        Just (Solved a, _) -> return a

currentCost :: MonadLogic m => w -> m (Step w a) -> m (Step w a)
currentCost w m = reflect (Just (Continue w, m))

weightedChoice :: (MonadLogic m, Ord w) => [m (Step w a)] -> m (Step w a)
weightedChoice = foldl' weightedInterleave empty

findN :: MonadLogic m => Int -> Int -> m (Step Int Int)
findN end start = currentCost (abs $ end - start) $ do
    if (end == start)
       then return (Solved start)
       else weightedChoice
              [ findN end (start * 10)
              , findN end (start * 3)
              , findN end (start - 1)
              , findN end (start + 2)
              ]
