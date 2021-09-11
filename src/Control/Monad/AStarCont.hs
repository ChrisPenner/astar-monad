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
    deriving newtype (Functor, Applicative, Monad)

instance (Monad m, Ord c) => Alternative (AStarT r c m) where
  empty = AStarT . lift . shiftT $ \_cc -> do
         pure Dead
  AStarT l <|> AStarT r = AStarT $ do
      c <- ask
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
      AStarT . lift $ shiftT $ \cc -> do
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

runAStar :: Monoid c => AStarT r c Identity r -> Resume c Identity r
runAStar = runIdentity . runAStarT

runAStarT :: (Monad m, Monoid c) => AStarT r c m r -> m (Resume c m r)
runAStarT (AStarT m) =
      flip runContT (pure . Done)
    . flip runReaderT mempty
    $ m

-- tester :: AStarT Int (Sum Int) Identity x
-- tester = do

    -- asum [spend 10 *> done 1, spend 10 *> done 2, done 3]
