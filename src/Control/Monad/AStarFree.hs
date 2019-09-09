{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.AStarFree where

import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Logic
import Control.Applicative
import Data.Functor.Identity
import Data.Semigroup
import Data.Bifunctor
import Control.Monad.Trans.Free

type AStar w r a = AStarT w r Identity a

data Step w r = Weighted w | Solved r
  deriving (Show, Eq)

newtype AStarT w r m a =
    AStarT { unAStarT :: FreeT ((,) (Step w r)) (LogicT m) a
          } deriving stock Functor

-- mapResult :: (r -> r') -> AStarT w r m a -> AStarT w r' m a
-- mapResult f (AStarT m) = AStarT $ fmap go m
--   where
--     go (Pure a) = Pure a
--     go (Weighted w) = Weighted w
--     go (Solved r) = Solved $ f r

instance MonadTrans (AStarT w r) where
  lift m = AStarT . lift . lift $ m

instance MonadIO m => MonadIO (AStarT w r m) where
  liftIO io = lift $ liftIO io

instance (Monad m) => Applicative (AStarT w r m) where
  pure = return
  (<*>) = ap

-- instance (Ord w, Monad m) => MonadPlus (AStarT w r m) where
--   mzero = empty
--   mplus = (<|>)

-- instance (Ord w, Monad m) => MonadFail (AStarT w r m) where
--   fail _ = empty

instance (Monad m) => Monad (AStarT w r m) where
  return = AStarT . pure
  AStarT m >>= f = AStarT $ do
      FreeT $ runFreeT m >>= \case
        Pure a -> runFreeT . unAStarT $ f a
        (Free (Solved r, _continue)) -> pure . Free $ (Solved r, empty)
        (Free (Weighted w, continue)) -> pure . Free $ (Weighted w, (continue >>= unAStarT . f))

-- instance (Ord w, Monad m) => Alternative (AStarT w r m) where
--   empty = AStarT empty
--   (<|>) = weightedInterleave

-- weightedInterleave :: (Ord w, Monad m) => AStarT w r m a -> AStarT w r m a -> AStarT w r m a
-- weightedInterleave (AStarT ma) (AStarT mb) = AStarT . lift $ do
--     rA <- msplit . runFreeT $ ma
--     rB <- msplit . runFreeT $ mb
--     case (rA, rB) of
--         (m, Nothing) -> _ $ reflect m

-- weightedInterleave' :: (Ord w, MonadLogic m) => m (FreeF (Step w r) _ _) -> m (Step w r a) -> m (Step w r a)
-- weightedInterleave' ma mb = do
--     rA <- msplit ma
--     rB <- msplit mb
--     case (rA, rB) of
--         (m, Nothing) -> reflect m
--         (Nothing, m) -> reflect m
--         (Just (Solved a, _), _) -> return (Solved a)
--         (_ , Just (Solved a, _)) -> return (Solved a)
--         (l@(Just (Weighted lw, lm)), r@(Just (Weighted rw, rm))) ->
--             if lw < rw
--                then pure (Weighted lw) <|> weightedInterleave' lm (reflect r)
--                else pure (Weighted rw) <|> weightedInterleave' rm (reflect l)
--         (Just (Pure{}, _), m) -> reflect m
--         (m, Just (Pure{}, _)) -> reflect m

-- runAStarT :: (Monad m) => AStarT w r m a -> m (Maybe r)
-- runAStarT (AStarT m) = fmap (fmap fst) . observeT . msplit $ do
--     m >>= \case
--       Solved a -> return a
--       _ -> empty
