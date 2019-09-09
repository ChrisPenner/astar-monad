{-# LANGUAGE RankNTypes #-}
module Control.Monad.AStar where

import Control.Monad.Logic
import Control.Applicative
import Data.Maybe
import Data.Foldable


data Step w a = Solved a | Continue w
    deriving Show

done :: Monad m => a -> m (Step w a)
done = return . Solved

continue :: Monad m => w -> m (Step w a) -> m (Step w a)
continue w ma = return (Continue w)

handleSingle :: MonadLogic m => Maybe (Step w a, m (Step w a)) -> m (Step w a)
handleSingle Nothing = empty
handleSingle (Just (Solved a, _)) = return (Solved a)
handleSingle (Just (Continue w, m)) = return (Continue w) `interleave` m

weightedInterleave :: (Ord w, MonadLogic m) => m (Step w a) -> m (Step w a) -> m (Step w a)
weightedInterleave a b = do
    rA <- msplit a
    rB <- msplit b
    case (rA, rB) of
        (m, Nothing) -> handleSingle m
        (Nothing, m) -> handleSingle m
        (Just (Solved a, _), _) -> return (Solved a)
        (_ , Just (Solved a, _)) -> return (Solved a)
        (l@(Just (Continue lw, lm)), r@(Just (Continue rw, rm))) ->
            if lw < rw
               then reflect (Just (Continue lw, weightedInterleave lm (reflect r)))
               else reflect (Just (Continue rw, weightedInterleave rm (reflect l)))


astar :: MonadLogic m => m (Step w a) -> m a
astar m = do
    r <- msplit m
    case r of
        Nothing -> empty
        Just (Continue _, m) -> astar m
        Just (Solved a, _) -> return a

findN :: MonadLogic m => Int -> Int -> m (Step Int Int)
findN end start = do
    if (end == start)
       then (return (Solved start) `interleave` empty)
       else
            (reflect (Just (Continue $ abs (end - (start * 5)), findN end (start * 5))))
            `weightedInterleave`
            (reflect (Just (Continue $ abs (end - (start - 1)), findN end (start - 1))))
            `weightedInterleave`
            (reflect (Just (Continue $ abs (end - (start + 2)), findN end (start + 2))))

