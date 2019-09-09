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
import Data.Functor.Identity
import Data.Semigroup
import Data.Bifunctor

data Step w r a = Pure a | Weighted w | Solved r
    deriving (Show, Functor, Eq)

type AStar w r a = AStarT w r Identity a

newtype AStarT w r m a =
    AStarT { unAStarT :: (LogicT m) (Step w r a)
          } deriving stock Functor

mapResult :: (r -> r') -> AStarT w r m a -> AStarT w r' m a
mapResult f (AStarT m) = AStarT $ fmap go m
  where
    go (Pure a) = Pure a
    go (Weighted w) = Weighted w
    go (Solved r) = Solved $ f r

instance MonadTrans (AStarT w r) where
  lift m = AStarT . lift $ (Pure <$> m)

instance (MonadIO m, Ord w) => MonadIO (AStarT w r m) where
  liftIO io = lift $ liftIO io

instance (Monad m, Ord w) => Applicative (AStarT w r m) where
  pure = return
  (<*>) = ap

instance (Ord w, Monad m) => MonadPlus (AStarT w r m) where
  mzero = empty
  mplus = (<|>)

instance (Ord w, Monad m) => MonadFail (AStarT w r m) where
  fail _ = empty

instance (Monad m, Ord w) => Monad (AStarT w r m) where
  return = AStarT . return . Pure
  AStarT m >>= f = AStarT $ do
      msplit m >>= \case
        Nothing -> empty
        Just (Pure a, continue) -> unAStarT $ (f a) `weightedInterleave` (AStarT continue >>= f)
        Just (Solved r, _) -> pure $ Solved r
        Just (Weighted w, continue) ->
            reflect $ Just (Weighted w, unAStarT $ AStarT continue >>= f)

instance (Ord w, Monad m) => Alternative (AStarT w r m) where
  empty = AStarT empty
  (<|>) = weightedInterleave

weightedInterleave :: (Ord w, Monad m) => AStarT w r m a -> AStarT w r m a -> AStarT w r m a
weightedInterleave (AStarT a) (AStarT b) = AStarT $ weightedInterleave' a b

weightedInterleave' :: (Ord w, MonadLogic m) => m (Step w r a) -> m (Step w r a) -> m (Step w r a)
weightedInterleave' ma mb = do
    rA <- msplit ma
    rB <- msplit mb
    case (rA, rB) of
        (m, Nothing) -> reflect m
        (Nothing, m) -> reflect m
        (Just (Solved a, _), _) -> return (Solved a)
        (_ , Just (Solved a, _)) -> return (Solved a)
        (l@(Just (Weighted lw, lm)), r@(Just (Weighted rw, rm))) ->
            if lw < rw
               then pure (Weighted lw) <|> weightedInterleave' lm (reflect r)
               else pure (Weighted rw) <|> weightedInterleave' rm (reflect l)
        (Just (Pure{}, _), m) -> reflect m
        (m, Just (Pure{}, _)) -> reflect m

runAStarT :: (Monad m) => AStarT w r m a -> m (Maybe r)
runAStarT (AStarT m) = fmap (fmap fst) . observeT . msplit $ do
    m >>= \case
      Solved a -> return a
      _ -> empty

runAStar :: AStar w r a -> Maybe r
runAStar = runIdentity . runAStarT

debugAStar :: AStar w r a -> ([w], Maybe r)
debugAStar = runIdentity . debugAStarT

debugAStarT :: (Monad m) => AStarT w r m a -> m ([w], Maybe r)
debugAStarT = astarWhile (const True)

astarWhile :: Monad m => (w -> Bool) -> AStarT w r m a -> m ([w], Maybe r)
astarWhile p m = do
    stepAStar m >>= \case
      Nothing -> return ([], Nothing)
      Just (Pure _, continue) -> astarWhile p continue
      Just (Weighted w, continue) ->
          if p w then first (w:) <$> astarWhile p continue
                 else return ([], Nothing)
      Just (Solved r, _) -> return ([], Just r)

stepAStar :: (Monad m) => AStarT w r m a -> m (Maybe (Step w r a, AStarT w r m a))
stepAStar (AStarT m) = fmap (fmap $ second AStarT) . observeT $ msplit m

done :: r -> AStarT w r m a
done = AStarT . pure . Solved

updateCost :: Monad m => w -> AStarT w r m ()
updateCost w = AStarT $ pure (Weighted w) <|> return (Pure ())

measure :: (Monad m, Ord w) => (a -> m (Either w r)) -> a -> AStarT w r m ()
measure eval a = lift (eval a) >>= either updateCost done

measure' :: (Monad m, Ord w) => (a -> m (Either w r)) -> a -> AStarT (Arg w a) r m ()
measure' eval = measure (\a -> fmap (first (flip Arg a)) $ eval a)

searchEq :: Eq a => a -> (a -> w) -> a -> Maybe w
searchEq dest f a
    | dest == a = Nothing
    | otherwise = Just $ f a
