{-|
Module      : Control.Monad.AStar
Description : Provides a monad transformer for A* search. See README for details and examples.
Copyright   : (c) Chris Penner, 2019
License     : BSD3

See the <https://github.com/chrispenner/astar-monad README> for usage info and examples.

-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.AStar
    (
    -- * Types
      AStar
    , AStarT
    , BranchState (..)

    -- * Methods
    , MonadAStar(..)
    , branch
    , failure

    -- * Executing Search
    , runAStarT
    , runAStar

    -- , tryWhile
    -- , tryWhileT
    )
    where

import Control.Monad.AStar.Class
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Logic
import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.Maybe
import Debug.Trace

data Step c r a = Pure a | Checkpoint | Solved
    deriving (Show, Functor, Eq)

data BranchState s c r =
    BranchState { branchState :: s
                , cost        :: c
                , result      :: Maybe r
                }
                deriving (Show, Eq)

-- | Non-transformer version of 'AStarT'
type AStar s c r a = AStarT s c r Identity a

-- | The 'AStar' search monad transformer
--
-- Lots of type variables here:
--
-- @s@: State; keep anything you want in here, it will stay coherent across
-- branch switches.
--
-- @c@: Cost measure: The type you'll use for determining the estimated cost of following a given
-- branch. Usually requires 'Ord'.
--
-- @r@: Result type, this is often redundant to State but is provided for convenience.
-- This is the type you pass to 'done' when you've found a solution.
--
-- @m@: An arbitrary monad which will be threaded through.
--
-- Be wary that effects will be run in seemingly non-deterministic ordering as we switch
-- chaotically between branches.
newtype AStarT s c r m a =
    AStarT { unAStarT :: StateT (BranchState s c r) (LogicT m) (Step c r a)
          } deriving stock Functor

-- mapResult :: (r -> r') -> AStarT s c r m a -> AStarT s c r' m a
-- mapResult f (AStarT m) = AStarT $ fmap go m
--   where
--     go (Pure a) = Pure a
--     go (Weighted c) = Weighted c
--     go (Solved r) = Solved $ f r

instance MonadTrans (AStarT s c r) where
  lift m = AStarT . lift . lift $ (Pure <$> m)

instance (MonadIO m, Ord c) => MonadIO (AStarT s c r m) where
  liftIO io = lift $ liftIO io

instance (Monad m, Ord c) => Applicative (AStarT s c r m) where
  pure = return
  (<*>) = ap

instance (Ord c, Monad m) => MonadPlus (AStarT s c r m) where
  mzero = empty
  mplus = (<|>)

instance (Ord c, Monad m) => MonadFail (AStarT s c r m) where
  fail _ = empty

instance (Ord c, Monad m) => MonadState s (AStarT s c r m) where
  get = AStarT $ Pure . branchState <$> get
  put s = AStarT $ Pure <$> modify (\bs -> bs{branchState=s})

instance (Monad m, Ord c) => Monad (AStarT s c r m) where
  return = AStarT . return . Pure
  AStarT m >>= f = AStarT $ do
      next <- msplit m
      bs <- get
      case next of
          Nothing -> empty
          (Just (Solved, _)) -> put bs >> pure Solved
          (Just (Pure a, continue)) -> unAStarT $ (f a) `weightedInterleave` (AStarT continue >>= f)
          (Just (Checkpoint, continue)) ->
                reflect $ Just (Checkpoint, unAStarT $ AStarT continue >>= f)

isSolved :: BranchState c r s -> Bool
isSolved = isJust . result

-- instance (Ord c, Monad m) => MonadLogic (AStarT s c r m) where
--   msplit (AStarT (StateT m)) = AStarT . StateT $ \s -> do
--       msplit (m s) >>= \case
--         (Just ((Weighted w, s), continue)) -> return (Weighted w, s)
--         (Just ((stp, s), continue)) -> return (_, s)
--         Nothing -> return $ (Pure Nothing, s)

instance (Ord c, Monad m) => Alternative (AStarT s c r m) where
  empty = AStarT empty
  (<|>) = weightedInterleave

weightedInterleave :: (Ord c, Monad m) => AStarT s c r m a -> AStarT s c r m a -> AStarT s c r m a
weightedInterleave (AStarT a) (AStarT b) = AStarT $ weightedInterleave' a b

weightedInterleave' :: (Ord c, MonadLogic m, MonadState (BranchState s c r) m) => m (Step c r a) -> m (Step c r a) -> m (Step c r a)
weightedInterleave' ma mb = do
    beforeBoth <- get
    (rA, lState) <- liftA2 (,) (msplit ma) get
    put beforeBoth
    (rB, rState) <- liftA2 (,) (msplit mb) get
    case (rA, rB) of
        (m, Nothing) -> put lState >> reflect m
        (Nothing, m) -> put rState >> reflect m
        (Just (Solved, _), _) -> put lState >> pure Solved
        (_, Just (Solved, _)) -> put rState >> pure Solved
        (l@(Just (Checkpoint, lm)), r@(Just (Checkpoint, rm)))
          | cost lState < cost rState ->do
              (put lState >> pure Checkpoint)
                <|> ((put lState >> lm) `weightedInterleave'` (put rState >> reflect r))
          | otherwise ->
              (put rState >> pure Checkpoint)
               <|> ((put rState >> rm) `weightedInterleave'` (put lState >> reflect l))
        ((Just (Pure la, lm)), r) ->
            (put lState >> pure (Pure la)) `interleave` ((put lState >> lm) `weightedInterleave'` (put rState >> reflect r))
        (l, (Just (Pure ra, rm))) ->
            (put rState >> pure (Pure ra)) `interleave` ((put rState >> rm) `weightedInterleave'` (put lState >> reflect l))

-- | Run an A* computation effect returning the solution and branch state if one was found.
runAStarT :: (Monad m, Monoid c) => AStarT s c r m a -> s -> m (Maybe (BranchState s c r))
runAStarT (AStarT m) s = fmap listToMaybe . observeManyT 1 $ do
    runStateT m (BranchState s mempty Nothing) >>= \case
          (Solved, s) -> return s
          _ -> empty

-- | Run a pure A* computation returning the solution and branch state if one was found.
runAStar :: (Monoid c) => AStar s c r a -> s -> Maybe (BranchState s c r)
runAStar m s = runIdentity $ runAStarT m s

-- | Run a pure A* search but short-circuit when the lowest cost fails a predicate.
--
-- This is useful for detecting if your search is diverging, or is likely to fail.
-- tryWhile :: Monoid c => (c -> Bool) -> AStar s c r a -> BranchState s c r -> (Maybe (BranchState s c r))
-- tryWhile p m s = runIdentity $ tryWhileT p m s

-- | Effectful version of 'tryWhile'
-- tryWhileT :: (Monoid c, Monad m) => (c -> Bool) -> AStarT s c r m a -> BranchState s c r -> m (Maybe (BranchState s c r))
-- tryWhileT p m s = do
--     stepAStar m s >>= \case
--       Nothing -> return Nothing
--       Just ((Pure _, s), continue) -> tryWhileT p continue s
--       Just ((Weighted c, s), continue) ->
--           if p c then tryWhileT p continue s
--                  else return Nothing
--       Just ((Solved r, s), _) -> return (Just s)

-- NOTE probably doesn't handle state properly in returned continuation...
stepAStar :: (Monoid c, Monad m) => AStarT s c r m a -> BranchState s c r -> m (Maybe ((Step c r a, BranchState s c r), AStarT s c r m a))
stepAStar (AStarT m) s = fmap (fmap go) . observeT . (fmap . fmap . fmap . fmap) fst $ msplit (runStateT m s)
  where
    go (v, x) = (v, AStarT (lift x))

instance (Ord w, Monad m) => MonadAStar w r (AStarT s w r m) where
  updateCost c = AStarT $ modify (\bs -> bs{cost=c}) >> (pure Checkpoint <|> pure (Pure ()))
  done r = AStarT $ modify (\bs -> bs{result=Just r}) >> pure Solved
