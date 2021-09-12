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
import Control.Monad.Trans.Cont
import Control.Applicative as Alt
import Control.Monad.Identity
import Data.Monoid
import Debug.Trace (traceM)

tick :: MonadState Int m => m Int
tick = do
    n <- get
    modify succ
    pure n

data Resume c m r =
    Resume c (ReaderT c m (Resume c m r))
      | Done r
      | Dead

instance (Show c, Show r) => Show (Resume c m r) where
  show (Done r) = "Done " <> show r
  show (Dead) = "Dead"
  show (Resume c _) = "Resume " <> show c

newtype AStarT r c m a =
    AStarT {unwrapAStarT :: (ContT (Resume c m r) (ReaderT c m)) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (Show c, Show r,  Monad m, Ord c, Semigroup c) => Alternative (AStarT r c m) where
  empty = AStarT . shiftT $ \_cc -> do
         pure Dead
  AStarT l <|> AStarT r = AStarT $ loop l r

loop :: (Monad m, Ord c, Semigroup c, Show c, Show r) =>
     ContT (Resume c m r) (ReaderT c m) a
  -> ContT (Resume c m r) (ReaderT c m) a
  -> ContT (Resume c m r) (ReaderT c m) a
loop l r = do
    shiftT $ \cc -> do
          ll <- resetT $ l >>= lift . cc
          lr <- resetT $ r >>= lift . cc
          case (ll, lr) of
            (Done r, _) -> pure (Done r)
            (_, Done r) -> pure (Done r)
            (Dead, Dead) -> do
                traceM "empty both"
                shiftT $ \_cc -> pure Dead
            (r, Dead) -> do
                traceM "emptyR"
                pure r
                -- lift $ actL
            (Dead, r) -> do
                traceM "emptyL"
                pure r
                -- lift $ actR 
                -- lift $ actR
            (resL@(Resume cl actL), resR@(Resume cr actR)) -> do
                traceM $ show (resL, resR)
                if cl <= cr then traceM "picked L" >> loop (lift $ actL) (pure resR)
                            else traceM "picked R" >> loop (lift $ actR) (pure resL)

instance (Show r, Monoid c, Ord c, Monad m, Show c) => MonadAStar c r (AStarT r c m) where
  spend c = do
      AStarT . shiftT $ \cc -> do
        cost <- ask
        pure $ Resume (cost <> c) (local (c <>) $ cc ())
  -- estimate c = AStarT $ local (const c) . unwrapAStarT $ spend mempty
  estimate _c = undefined
  done r = AStarT . shiftT $ \_cc -> do
      pure (Done r)

loop1 :: (Monad m, Semigroup c) => Resume c m r -> ReaderT c m (Maybe r)
loop1 = \case
  Done r -> pure (Just r)
  Dead -> pure Nothing
  Resume _ act -> act >>= loop1

runAStar :: Monoid c => AStarT r c Identity r -> Maybe r
runAStar = runIdentity . runAStarT

runAStarT :: (Monad m, Monoid c) => AStarT r c m r -> m (Maybe r)
runAStarT (AStarT m) = flip runReaderT mempty $ do
    unwrapped <- flip runContT (pure . Done) $ m
    loop1 unwrapped

tester :: AStarT Int (Sum Int) IO Int
tester = do
    spend 100
    -- printCost
    -- asum [spend 10 *> pure 1, spend 10 *> pure 2]
    -- (spend 20 *> pure 2) <|> (spend 10 *> pure 1) <|> (spend 20 *> pure 2)
    r <- empty <|> (spend 10 *> printCost *> spend 8 *> pure 10) <|> empty <|> (spend 5 *> spend 900 *> spend 7 *> printCost *> pure 5)
    spend 1 <|> spend 2
    pure r



printCost :: Show c => AStarT r c IO ()
printCost = AStarT ask >>= liftIO . print




testR :: Monad m => ContT a (ReaderT Int m) ()
testR = do
    shiftT $ \cc ->
        lift . local (+ 10) $ cc ()

thingy :: IO ()
thingy =
    flip runReaderT 0 $ flip runContT pure $ testR >> resetT (ask >>= liftIO . print)
