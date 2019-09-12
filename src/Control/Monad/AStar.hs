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
    , runAStar
    , runAStarT
    , evalAStar
    , evalAStarT
    , execAStar
    , execAStarT

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

data Step r a = Pure a | Checkpoint | Solved r
    deriving (Show, Functor, Eq)

data BranchState s c =
    BranchState { branchState      :: s
                , cumulativeCost   :: c
                , estimateTillDone :: c
                }
    deriving (Show, Eq)

totalCost :: Semigroup c => BranchState s c -> c
totalCost bs = cumulativeCost bs <> estimateTillDone bs

-- | Non-transformer version of 'AStarT'
type AStar s c r a = AStarT s c r Identity a

-- | The 'AStar' search monad transformer
--
-- Lots of type variables here:
--
-- @s@: State; keep anything you want in here, it will stay coherent across
-- branch switches.
--
-- @c@: Cost measure: The type you'll use for both your heuristic estimate and for your
-- accumulated total cost.
-- Usually requires 'Ord' (for comparing branch costs) and 'Monoid' (for appending and
-- instantiating cumulative costs).
--
-- @r@: Result type, this is often redundant to State but is provided for convenience.
-- This is the type you pass to 'done' when you've found a solution.
--
-- @m@: An arbitrary monad which will be threaded through.
--
-- Be wary that effects will be run in seemingly non-deterministic ordering as we switch
-- chaotically between branches.
newtype AStarT s c r m a =
    AStarT { unAStarT :: StateT (BranchState s c) (LogicT m) (Step r a)
          } deriving stock Functor

instance MonadTrans (AStarT s c r) where
  lift m = AStarT . lift . lift $ (Pure <$> m)

instance (MonadIO m, Semigroup c, Ord c) => MonadIO (AStarT s c r m) where
  liftIO io = lift $ liftIO io

instance (Monad m, Semigroup c, Ord c) => Applicative (AStarT s c r m) where
  pure = return
  (<*>) = ap

instance (Ord c, Semigroup c, Monad m) => MonadPlus (AStarT s c r m) where
  mzero = empty
  mplus = (<|>)

instance (Ord c, Semigroup c, Monad m) => MonadFail (AStarT s c r m) where
  fail _ = empty

instance (Ord c, Semigroup c, Monad m) => MonadState s (AStarT s c r m) where
  get = AStarT $ Pure . branchState <$> get
  put s = AStarT $ Pure <$> modify (\bs -> bs{branchState=s})

instance (Monad m, Semigroup c, Ord c) => Monad (AStarT s c r m) where
  return = AStarT . return . Pure
  AStarT m >>= f = AStarT $ do
      next <- msplit m
      case next of
          Nothing -> empty
          (Just (Solved r, _)) -> pure (Solved r)
          -- Should I interleave these instead?
          (Just (Pure a, continue)) -> (unAStarT $ f a) <|> unAStarT (AStarT continue >>= f)
          (Just (Checkpoint, continue)) ->
                pure Checkpoint <|> unAStarT (AStarT continue >>= f)

instance (Ord c, Monad m, Semigroup c) => Alternative (AStarT s c r m) where
  empty = AStarT empty
  (<|>) = weightedInterleave

weightedInterleave :: (Ord c, Semigroup c, Monad m) => AStarT s c r m a -> AStarT s c r m a -> AStarT s c r m a
weightedInterleave (AStarT a) (AStarT b) = AStarT $ weightedInterleave' a b

weightedInterleave' :: (Ord c, Semigroup c, MonadLogic m, MonadState (BranchState s c) m) => m (Step r a) -> m (Step r a) -> m (Step r a)
weightedInterleave' ma mb = do
    beforeBoth <- get
    (rA, lState) <- liftA2 (,) (msplit ma) get
    put beforeBoth
    (rB, rState) <- liftA2 (,) (msplit mb) get
    case (rA, rB) of
        (m, Nothing) -> put lState >> reflect m
        (Nothing, m) -> put rState >> reflect m
        (Just (Solved r, _), _) -> put lState >> pure (Solved r)
        (_, Just (Solved r, _)) -> put rState >> pure (Solved r)
        (l@(Just (Checkpoint, lm)), r@(Just (Checkpoint, rm)))
          | totalCost lState <= totalCost rState ->do
              (put lState >> pure Checkpoint)
                <|> ((put lState >> lm) `weightedInterleave'` (put rState >> reflect r))
          | otherwise ->
              (put rState >> pure Checkpoint)
               <|> ((put rState >> rm) `weightedInterleave'` (put lState >> reflect l))
        ((Just (Pure la, lm)), r) ->
            (put lState >> pure (Pure la)) `interleave` ((put lState >> lm) `weightedInterleave'` (put rState >> reflect r))
        (l, (Just (Pure ra, rm))) ->
            (put rState >> pure (Pure ra)) `interleave` ((put rState >> rm) `weightedInterleave'` (put lState >> reflect l))

-- | Run a pure A* computation returning the solution and branch state if one was found.
runAStar :: (Monoid c) => AStar s c r a -> s -> Maybe (r, s)
runAStar m s = runIdentity $ runAStarT m s

-- | Run an A* computation effect returning the solution and branch state if one was found.
runAStarT :: (Monad m, Monoid c) => AStarT s c r m a -> s -> m (Maybe (r, s))
runAStarT (AStarT m) s = fmap listToMaybe . observeManyT 1 $ do
    runStateT m (BranchState s mempty mempty) >>= \case
          (Solved r, s) -> return (r, branchState s)
          _ -> empty

-- | Like 'runAStar' but returns only the result
evalAStar :: (Monoid c) => AStar s c r a -> s -> Maybe r
evalAStar m s = fst <$> runAStar m s

-- | Like 'runAStarT' but returns only the result
evalAStarT :: (Monad m, Monoid c) => AStarT s c r m a -> s -> m (Maybe r)
evalAStarT m s = fmap fst <$> runAStarT m s

-- | Like 'runAStar' but returns only the state
execAStar :: (Monoid c) => AStar s c r a -> s -> Maybe s
execAStar m s = fmap snd $ runAStar m s

-- | Like 'execAStarT' but returns only the state
execAStarT :: (Monad m, Monoid c) => AStarT s c r m a -> s -> m (Maybe s)
execAStarT m s = fmap snd <$> runAStarT m s

-- | Run a pure A* search but short-circuit when the lowest cost fails a predicate.
--
-- This is useful for detecting if your search is diverging, or is likely to fail.
-- tryWhile :: Monoid c => (s -> c -> Bool) -> AStar s c r a -> s -> (Maybe (r, s))
-- tryWhile p m s = runIdentity $ tryWhileT p m s

-- | Effectful version of 'tryWhile'
-- tryWhileT :: (Monoid c, Monad m) => (s -> c -> Bool) -> AStarT s c r m a -> s -> m (Maybe (r, s))
-- tryWhileT p m s = fmap (second branchState) <$> tryWhileT' p m (BranchState s mempty mempty)

-- tryWhileT' :: (Monoid c, Monad m) => (s -> c -> Bool) -> AStarT s c r m a -> BranchState s c -> m (Maybe (r, BranchState s c))
-- tryWhileT' p m s = do
--     stepAStar m s >>= \case
--       Nothing -> return Nothing
--       Just ((Pure _, s), continue) -> tryWhileT' p continue s
--       Just ((Checkpoint, bs), continue) -> do
--           if p (branchState bs) (totalCost bs) then tryWhileT' p continue s
--                                                else return Nothing
--       Just ((Solved r, s), _) -> return (Just (r, s))

-- NOTE probably doesn't handle state properly in returned continuation...
-- stepAStar :: (Monoid c, Monad m) => AStarT s c r m a -> BranchState s c -> m (Maybe ((Step r a, BranchState s c), AStarT s c r m a))
-- stepAStar (AStarT m) s = fmap (fmap go) . observeT . _ $ msplit (evalStateT m s)
--   where
--     go (v, x) = (v, AStarT (lift x))

instance (Ord c, Monoid c, Monad m) => MonadAStar c r (AStarT s c r m) where
  estimate c = AStarT $ modify (\bs -> bs{estimateTillDone=c}) >> (pure Checkpoint <|> pure (Pure ()))
  spend c = AStarT $ modify (\bs -> bs{cumulativeCost=cumulativeCost bs <> c}) >> (pure Checkpoint <|> pure (Pure ()))
  done = AStarT . pure . Solved
