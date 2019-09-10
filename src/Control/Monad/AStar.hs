{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Control.Monad.AStar where

import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Logic
import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.Semigroup
import Data.Bifunctor
import Data.Maybe

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
    (rA, lState) <- liftA2 (,) (msplit ma) get
    put beforeBoth
    (rB, rState) <- liftA2 (,) (msplit mb) get
    case (rA, rB) of
        (m, Nothing) -> put lState >> reflect m
        (Nothing, m) -> put rState >> reflect m
        (Just (Solved a, _), _) -> put lState >> pure (Solved a)
        (_ , Just (Solved a, _)) -> put rState >> pure (Solved a)
        (l@(Just (Weighted lw, lm)), r@(Just (Weighted rw, rm)))
          | lw < rw ->
              (put lState >> pure (Weighted lw))
                <|> ((put lState >> lm) `weightedInterleave'` (put rState >> reflect r))
          | otherwise ->
              (put rState >> pure (Weighted rw))
               <|> ((put rState >> rm) `weightedInterleave'` (put lState >> reflect l))
        (l, r) -> (put lState >> reflect l) `weightedInterleave'` (put rState >> reflect r)

order :: (MonadState s m, MonadLogic m, Ord w)
      => (s, s)
      -> (Step w r a, m (Step w r a))
      -> (Step w r a, m (Step w r a))
      -> m (Step w r a)
order (lState, rState) (a, am) (b, bm) =
    ((put lState >> pure a) <|> am) `weightedInterleave'` (put rState >> pure b <|> bm)

runAStarT :: (Monad m) => AStarT s w r m a -> s -> m (Maybe (r, s))
runAStarT (AStarT m) s = fmap listToMaybe . observeManyT 1 $ do
    runStateT m s >>= \case
      (Solved a, s) -> return (a, s)
      _ -> empty

runAStar :: AStar s w r a -> s -> Maybe (r, s)
runAStar m s = runIdentity $ runAStarT  m s

execAStarT :: (Monad m) => AStarT s w r m a -> s -> m (Maybe s)
execAStarT m s = fmap snd <$> runAStarT m s

evalAStarT :: (Monad m) => AStarT s w r m a -> s -> m (Maybe r)
evalAStarT m s = fmap fst <$> runAStarT m s

tryWhile :: Monad m => (w -> Bool) -> AStarT s w r m a -> AStarT s w r m a
tryWhile p (AStarT m) = AStarT $ do
    m >>= \case
      (Weighted w) | p w -> pure (Weighted w)
                   | otherwise -> empty
      x -> pure x


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
