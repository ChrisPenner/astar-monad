{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.AStar where

import Control.Monad.Logic
import Control.Applicative
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Semigroup
import Data.Bifunctor

data Step w r a = Pure a | Weighted w | Solved r
    deriving (Show, Functor, Eq)

type AStar r a = forall w. Ord w => AStarT w r Identity a

newtype AStarT w r m a =
    AStarT { unAStarT :: ReaderT (r -> Maybe w) (LogicT m) (Step w r a)
          } deriving Functor

instance (Monad m) => Applicative (AStarT w r m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (AStarT w r m) where
  return = AStarT . return . Pure
  AStarT m >>= f = AStarT $ do
      msplit m >>= \case
        Nothing -> empty
        Just (Pure a, _) -> unAStarT . f $ a
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

runAStarT :: (Monad m) => (r -> Maybe w) -> AStarT w r m a -> m (Maybe r)
runAStarT measurer (AStarT m) = fmap (fmap fst) . observeT . msplit . flip runReaderT measurer $ do
    m >>= \case
      Solved a -> return a
      _ -> empty

runAStar :: Ord w => (r -> Maybe w) -> AStar r a -> Maybe r
runAStar measurer = runIdentity . runAStarT measurer

debugAStar :: forall w r a. Ord w => (r -> Maybe w) -> AStar r a -> ([(w, r)], Maybe r)
debugAStar measurer = first (fmap argTuple) . runIdentity . astarWhile (const True) annotatedMeasure
  where
    annotatedMeasure :: r -> Maybe (Arg w r)
    annotatedMeasure r = Arg <$> measurer r <*> return r
    argTuple (Arg a b) = (a, b)

astarWhile :: Monad m => (w -> Bool) -> (r -> Maybe w) -> AStarT w r m a -> m ([w], Maybe r)
astarWhile p measurer m = do
    stepAStar measurer m >>= \case
      Nothing -> return ([], Nothing)
      Just (Pure _, continue) -> astarWhile p measurer continue
      Just (Weighted w, continue) ->
          if p w then first (w:) <$> astarWhile p measurer continue
                 else return ([], Nothing)
      Just (Solved r, _) -> return ([], Just r)

stepAStar :: (Monad m) => (r -> Maybe w) -> AStarT w r m a -> m (Maybe (Step w r a, AStarT w r m a))
stepAStar measurer (AStarT m) = fmap (fmap $ second AStarT) . observeT . flip runReaderT measurer $ msplit m

measure :: (Monad m) => r -> AStarT w r m ()
measure r = AStarT $ do
    eval <- ask
    case eval r of
        Nothing -> pure (Solved r)
        (Just w) -> reflect (Just (Weighted w, return (Pure ())))

searchEq :: Eq a => a -> (a -> w) -> a -> Maybe w
searchEq dest f a
    | dest == a = Nothing
    | otherwise = Just $ f a
