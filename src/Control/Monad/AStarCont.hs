{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Control.Monad.AStarCont where
-- ( AStarT
--     , runAStarT
--     , findFirst
--     , observeAll
--     )

import Control.Monad.AStar.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.IntPSQ as PSQ
import Control.Monad.Trans.Cont
import Control.Applicative as Alt
import Control.Monad.Identity
import Data.Monoid
import Data.Foldable

tick :: MonadState Int m => m Int
tick = do
    n <- get
    modify succ
    pure n

type Q c v = IntPSQ c v

data Resume c m r =
    Resume c (m (Resume c m r))
      | Done r
      | Dead

newtype AStarT r c m a =
    AStarT {unwrapAStarT :: ReaderT c (ContT (Resume c m r) m) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Monad m, Ord c) => Alternative (AStarT r c m) where
  empty = AStarT . lift . shiftT $ \_cc -> do
         pure Dead
  AStarT l <|> AStarT r = do
      c <- AStarT $ ask
      loop c l r

loop :: (Monad m, Ord c) =>
  c
  -> ReaderT c (ContT (Resume c m r) m) a
  -> ReaderT c (ContT (Resume c m r) m) a
  -> AStarT r c m a
loop c l r = AStarT $ do
      lift . shiftT $ \cc -> do
          ll <- resetT $ runReaderT l c >>= lift . cc
          lr <- resetT $ runReaderT r c >>= lift . cc
          case (ll, lr) of
            (Done r, _) -> pure (Done r)
            (_, Done r) -> pure (Done r)
            (Dead, Dead) -> shiftT $ \_cc -> pure Dead
            (Resume _ actL, Dead) ->
                lift $ actL
            (Dead, Resume _ actR) ->
                lift $ actR
            (resL@(Resume cl actL), resR@(Resume cr actR)) ->
                if cl <= cr then flip runReaderT c $ unwrapAStarT (AStarT (lift . lift $ actL) <|> pure resR)
                            else flip runReaderT c $ unwrapAStarT (AStarT (lift . lift $ actR) <|> pure resL)

instance (Monoid c, Ord c, Monad m) => MonadAStar c r (AStarT r c m) where
  spend c = do
      cost <- AStarT $ ask
      AStarT . local (<> c) $ lift $ shiftT $ \cc -> do
        pure $ Resume (cost <> c) (cc ())
  -- estimate c = AStarT $ local (const c) . unwrapAStarT $ spend mempty
  estimate c = undefined
  done r = AStarT . lift $ shiftT $ \_cc -> do
      pure (Done r)

-- stash :: (Semigroup c, Ord c, Monad m) => c -> AStarT r c m (Maybe r) -> AStarT r c m ()
-- stash c cc = AStarT $ do
--   n <- lift . lift $ tick
--   p <- asks (<> c)
--   modify (PSQ.insert n p cc)

-- next :: (Monad m, Semigroup c, Ord c) => c -> AStarT r c m (Maybe r) -> AStarT r c m (Maybe r)
-- next c cc = do
--     stash c cc
--     q <- AStarT $ get
--     let (result, newQ) = flip alterMin q $ \case
--                             Just (_, _, nextAction) -> (nextAction, Nothing)
--                             Nothing -> (pure Nothing, Nothing)
--     AStarT $ put newQ
--     result

loop1 :: Monad m => Resume c m r -> m (Maybe r)
loop1 = \case
  Done r -> pure (Just r)
  Dead -> pure Nothing
  Resume _ act -> act >>= loop1

runAStar :: Monoid c => AStarT r c Identity r -> Maybe r
runAStar = runIdentity . runAStarT

runAStarT :: (Monad m, Monoid c) => AStarT r c m r -> m (Maybe r)
runAStarT (AStarT m) = do
    unwrapped <- flip runContT (pure . Done)
                . flip runReaderT mempty
                $ m
    loop1 unwrapped


shower :: Show r => Resume c m r -> [Char]
shower (Resume _ _) = "Resume"
shower (Done r) = show r
shower Dead = "Dead"

tester :: AStarT Int (Sum Int) IO x
tester = do
    spend 100
    asum [spend 10 *> done 1, spend 80 *> printCost *> done 2, spend 20 *> tester]


printCost :: Show c => AStarT r c IO ()
printCost = AStarT ask >>= liftIO . print

