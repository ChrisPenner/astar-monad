{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Control.Monad.AStar where

import Control.Monad.Logic
import Control.Applicative
import Data.Maybe
import Data.Foldable
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Semigroup

type AStar w r a = AStarT w r Identity a

newtype AStarT w r m a =
    AStarT { unAStarT :: ReaderT (r -> Maybe w) (LogicT m) (Step w r a)
          } deriving Functor

instance (Monad m) => Applicative (AStarT w r m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (AStarT w r m) where
  return = AStarT . return . Pure
  AStarT m >>= f = AStarT $ do
      eval <- ask
      msplit m >>= \case
        Nothing -> empty
        Just (Pure a, _) -> unAStarT . f $ a
        Just (Solved r, _) -> pure $ Solved r
        Just (Weighted w, m) -> reflect $ Just (Weighted w, unAStarT $ AStarT m >>= f)
        -- Just (Continue r, m) -> do
        --     case eval r of
        --         Nothing -> pure (Solved r)
        --         (Just w) -> reflect $ Just (Weighted w, unAStarT $ AStarT m >>= f)

runAStarT :: (Monad m) => (r -> Maybe w) -> AStarT w r m a -> m (Maybe r)
runAStarT measurer = fmap (fmap getFirst . foldMap go) . observeAllT . flip runReaderT measurer . unAStarT
  where
    go (Solved r) = Just (First r)
    go _ = Nothing

runAStar :: (r -> Maybe w) -> AStar w r a -> Maybe r
runAStar measurer = fmap getFirst . foldMap go . flip runReaderT measurer . unAStarT
  where
    go (Solved r) = Just (First r)
    go _ = Nothing

debugAStar :: (r -> Maybe w) -> AStar w r a -> [Step w r a]
debugAStar measurer = observeAll . flip runReaderT measurer . unAStarT
  where
    go (Solved r) = Just (First r)
    go _ = Nothing


data Step w r a = Pure a | Weighted w | Solved r
    deriving (Show, Functor)

measure :: (Monad m) => r -> AStarT w r m ()
measure r = AStarT $ do
    eval <- ask
    case eval r of
        Nothing -> pure (Solved r)
        (Just w) -> reflect (Just (Weighted w, return (Pure ())))

weightedInterleave :: (Ord w, Monad m) => AStarT w r m a -> AStarT w r m a -> AStarT w r m a
weightedInterleave (AStarT a) (AStarT b) = AStarT $ weightedInterleave' a b

weightedInterleave' :: (Ord w, MonadLogic m) => m (Step w r a) -> m (Step w r a) -> m (Step w r a)
weightedInterleave' a b = do
    rA <- msplit a
    rB <- msplit b
    case (rA, rB) of
        (m, Nothing) -> reflect m
        (Nothing, m) -> reflect m
        (Just (Solved a, _), _) -> return (Solved a)
        (_ , Just (Solved a, _)) -> return (Solved a)
        (l@(Just (Weighted lw, lm)), r@(Just (Weighted rw, rm))) ->
            if lw < rw
               then reflect (Just (Weighted lw, weightedInterleave' lm (reflect r)))
               else reflect (Just (Weighted rw, weightedInterleave' rm (reflect l)))

-- -- weightedInterleave :: (Ord w, MonadLogic m) => m (Step w a) -> m (Step w a) -> m (Step w a)
-- -- weightedInterleave a b = do
-- --     rA <- msplit a
-- --     rB <- msplit b
-- --     case (rA, rB) of
-- --         (m, Nothing) -> reflect m
-- --         (Nothing, m) -> reflect m
-- --         (Just (Solved a, _), _) -> return (Solved a)
-- --         (_ , Just (Solved a, _)) -> return (Solved a)
-- --         (l@(Just (Continue lw, lm)), r@(Just (Continue rw, rm))) ->
-- --             if lw < rw
-- --                then reflect (Just (Continue lw, weightedInterleave lm (reflect r)))
-- --                else reflect (Just (Continue rw, weightedInterleave rm (reflect l)))

-- solve :: MonadLogic m => m (Step w a) -> m a
-- solve m = do
--     r <- msplit m
--     case r of
--         Nothing -> empty
--         Just (Continue _, m) -> solve m
--         Just (Solved a, _) -> return a

-- currentCost :: MonadLogic m => w -> m (Step w a) -> m (Step w a)
-- currentCost w m = reflect (Just (Continue w, m))

weightedChoice :: (Ord w) => [AStar w r a] -> AStar w r a
weightedChoice = foldl' weightedInterleave (AStarT empty)

-- findN :: Int -> Int -> AStar Int Int Int
-- findN end start = currentCost (abs $ end - start) $ do
--     if (end == start)
--        then return (Solved start)
--        else weightedChoice
--               [ findN end (start * 10)
--               , findN end (start * 3)
--               , findN end (start - 1)
--               , findN end (start + 2)
--               ]


searchFor :: Int -> Int -> Maybe Int
searchFor end start
    | end == start = Nothing
searchFor end start = Just $ abs (end - start)

findN :: Int -> AStar Int Int ()
findN n = do
    measure n
    weightedChoice
        [ findN (n * 10)
        , findN (n * 3)
        , findN (n - 1)
        , findN (n + 2)
        ]
