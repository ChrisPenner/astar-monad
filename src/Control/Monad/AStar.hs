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

instance (Ord w, Monad m) => Alternative (AStarT w r m) where
  empty = AStarT empty
  (<|>) = weightedInterleave

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

findSolved :: Step w r a -> Maybe (First r)
findSolved (Solved r) = Just (First r)
findSolved _ = Nothing

runAStarT :: (Monad m) => (r -> Maybe w) -> AStarT w r m a -> m (Maybe r)
runAStarT measurer = fmap (fmap getFirst . foldMap findSolved) . observeAllT . flip runReaderT measurer . unAStarT

runAStar :: (r -> Maybe w) -> AStar w r a -> Maybe r
runAStar measurer = runIdentity . runAStarT measurer

debugAStar :: (r -> Maybe w) -> AStar w r a -> [Step w r a]
debugAStar measurer = observeAll . flip runReaderT measurer . unAStarT

data Step w r a = Pure a | Weighted w | Solved r
    deriving (Show, Functor)

measure :: (Monad m) => r -> AStarT w r m ()
measure r = AStarT $ do
    eval <- ask
    case eval r of
        Nothing -> pure (Solved r)
        (Just w) -> reflect (Just (Weighted w, return (Pure ())))

searchFor :: Int -> Int -> Maybe Int
searchFor end start
    | end == start = Nothing
searchFor end start = Just $ abs (end - start)

findN :: Int -> AStar Int Int ()
findN n = do
    measure n
    asum
        [ findN (n * 10)
        , findN (n * 3)
        , findN (n - 1)
        , findN (n + 2)
        ]
