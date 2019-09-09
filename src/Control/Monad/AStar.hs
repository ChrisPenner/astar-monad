{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.AStar where

import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Logic
import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.Semigroup
import Data.Bifunctor

data Step w r a = Pure a | Weighted w | Solved r
    deriving (Show, Functor, Eq)

type AStar s w r a = AStarT s w r Identity a

newtype AStarT s w r m a =
    AStarT { unAStarT :: StateT s (LogicT m) (Step w r a)
          } deriving stock Functor

mapResult :: (r -> r') -> AStarT s w r m a -> AStarT s w r' m a
mapResult f (AStarT m) = AStarT $ fmap go m
  where
    go (Pure a) = Pure a
    go (Weighted w) = Weighted w
    go (Solved r) = Solved $ f r

instance MonadTrans (AStarT s w r) where
  lift m = AStarT . lift . lift $ (Pure <$> m)

instance (MonadIO m, Ord w) => MonadIO (AStarT s w r m) where
  liftIO io = lift $ liftIO io

instance (Monad m, Ord w) => Applicative (AStarT s w r m) where
  pure = return
  (<*>) = ap

instance (Ord w, Monad m) => MonadPlus (AStarT s w r m) where
  mzero = empty
  mplus = (<|>)

instance (Ord w, Monad m) => MonadFail (AStarT s w r m) where
  fail _ = empty

instance (Ord w, Monad m) => MonadState s (AStarT s w r m) where
  get = AStarT $ Pure <$> get
  put s = AStarT $ Pure <$> put s

instance (Monad m, Ord w) => Monad (AStarT s w r m) where
  return = AStarT . return . Pure
  AStarT m >>= f = AStarT $ do
      msplit m >>= \case
        Nothing -> empty
        Just (Pure a, continue) -> unAStarT $ (f a) `weightedInterleave` (AStarT continue >>= f)
        Just (Solved r, _) -> pure $ Solved r
        Just (Weighted w, continue) -> do
            reflect $ Just (Weighted w, unAStarT $ AStarT continue >>= f)

instance (Ord w, Monad m) => Alternative (AStarT s w r m) where
  empty = AStarT empty
  (<|>) = weightedInterleave

weightedInterleave :: (Ord w, Monad m) => AStarT s w r m a -> AStarT s w r m a -> AStarT s w r m a
weightedInterleave (AStarT a) (AStarT b) = AStarT $ weightedInterleave' a b

weightedInterleave' :: (Ord w, MonadLogic m, MonadState s m) => m (Step w r a) -> m (Step w r a) -> m (Step w r a)
weightedInterleave' ma mb = do
    beforeBoth <- get
    (rA, aAfterState) <- liftA2 (,) (msplit ma) get
    put beforeBoth
    (rB, bAfterState) <- liftA2 (,) (msplit mb) get
    put beforeBoth
    case (rA, rB) of
        (m, Nothing) -> put aAfterState >> reflect m
        (Nothing, m) -> put bAfterState >> reflect m
        (Just (Solved a, _), _) -> put aAfterState >> return (Solved a)
        (_ , Just (Solved a, _)) -> put bAfterState >> return (Solved a)
        (l@(Just (Weighted lw, lm)), r@(Just (Weighted rw, rm))) ->
            if lw < rw
               then (put aAfterState >> pure (Weighted lw)) <|> weightedInterleave' (put aAfterState >> lm) (put bAfterState >> reflect r)
               else (put bAfterState >> pure (Weighted rw)) <|> weightedInterleave' (put bAfterState >> rm) (put aAfterState >> reflect l)
        (Just (Pure a, continue), m) -> (put aAfterState >> (pure (Pure a) <|> continue)) `weightedInterleave'` (put bAfterState >> reflect m)
        (m, Just (Pure a, continue)) -> (put aAfterState >> reflect m) `weightedInterleave'` (put bAfterState >> (pure (Pure a) <|> continue))

runAStarT :: (Monad m) => AStarT s w r m a -> s -> m (Maybe (r, s))
runAStarT (AStarT m) s = fmap (fmap fst) . observeT . msplit $ do
    runStateT m s >>= \case
      (Solved a, s) -> return (a, s)
      _ -> empty

runAStar :: AStar s w r a -> s -> Maybe (r, s)
runAStar m s = runIdentity $ runAStarT  m s

debugAStar :: AStar s w r a -> s -> (Maybe (r, s))
debugAStar m s = runIdentity $ debugAStarT m s

debugAStarT :: (Monad m) => AStarT s w r m a -> s -> m (Maybe (r, s))
debugAStarT m s = astarWhile (const True) m s

astarWhile :: Monad m => (w -> Bool) -> AStarT s w r m a -> s -> m (Maybe (r, s))
astarWhile p m s = do
    stepAStar m s >>= \case
      Nothing -> return Nothing
      Just ((Pure _, s), continue) -> astarWhile p continue s
      Just ((Weighted w, s), continue) ->
          if p w then astarWhile p continue s
                 else return Nothing
      Just ((Solved r, s), _) -> return (Just (r, s))

stepAStar :: (Monad m) => AStarT s w r m a -> s -> m (Maybe ((Step w r a, s), AStarT s w r m a))
stepAStar (AStarT m) s = fmap (fmap go) . observeT . (fmap . fmap . fmap . fmap) fst $ msplit (runStateT m s)
  where
    go (v, x) = (v, AStarT (lift x))

done :: r -> AStarT s w r m a
done = AStarT . pure . Solved

updateCost :: Monad m => w -> AStarT s w r m ()
updateCost w = AStarT $ pure (Weighted w) <|> return (Pure ())

measure :: (Monad m, Ord w) => (a -> m (Either w r)) -> a -> AStarT s w r m ()
measure eval a = lift (eval a) >>= either updateCost done

measure' :: (Monad m, Ord w) => (a -> m (Either w r)) -> a -> AStarT s (Arg w a) r m ()
measure' eval = measure (\a -> fmap (first (flip Arg a)) $ eval a)

searchEq :: Eq a => a -> (a -> w) -> a -> Maybe w
searchEq dest f a
    | dest == a = Nothing
    | otherwise = Just $ f a
