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
import Data.Foldable
import Control.Monad.Identity
import Data.Monoid

tick :: MonadState Int m => m Int
tick = do
    n <- get
    modify succ
    pure n

type Q c v = IntPSQ c v

newtype AStarT r c m a =
    AStarT {unwrapAStarT :: (ContT (Maybe r) (StateT (Q c (AStarT r c m (Maybe r))) (ReaderT c (StateT Int m))) a)}
    deriving newtype (Functor, Applicative, Monad)

instance (Monad m) => Alternative (AStarT r c m) where
  empty = AStarT $ do
      shiftT $ \_cc -> do
          pure Nothing
  AStarT l <|> AStarT r = AStarT $ do
      shiftT $ \cc -> do
        let x = runContT l cc
        let y = runContT r cc
        _


      -- AStarT $ do
      -- shiftT $ \cc -> do
      --     a <- l
      --     rl <- lift $ cc a
      --     b <- r
      --     rr <- lift $ cc b
      --     pure (rl <|> rr)

instance (Monoid c, Ord c, Monad m) => MonadAStar c r (AStarT r c m) where
  spend c = AStarT $ shiftT $ \cc -> do
      unwrapAStarT $ next c (AStarT . lift $ cc ())
  estimate c = AStarT $ local (const c) . unwrapAStarT $ spend mempty
  done r = AStarT $ shiftT $ \_cc -> do
      pure (Just r)

stash :: (Semigroup c, Ord c, Monad m) => c -> AStarT r c m (Maybe r) -> AStarT r c m ()
stash c cc = AStarT $ do
  n <- lift . lift $ tick
  p <- asks (<> c)
  modify (PSQ.insert n p cc)

next :: (Monad m, Semigroup c, Ord c) => c -> AStarT r c m (Maybe r) -> AStarT r c m (Maybe r)
next c cc = do
    stash c cc
    q <- AStarT $ get
    let (result, newQ) = flip alterMin q $ \case
                            Just (_, _, nextAction) -> (nextAction, Nothing)
                            Nothing -> (pure Nothing, Nothing)
    AStarT $ put newQ
    result

runAStar :: Monoid c => AStarT r c Identity r -> Maybe r
runAStar (AStarT m) =
      runIdentity
    . flip evalStateT 0
    . flip runReaderT mempty
    . flip evalStateT PSQ.empty
    . flip runContT (pure . pure)
    $ m

runAStarT :: (Monad m, Monoid c) => AStarT r c m r -> m (Maybe r)
runAStarT (AStarT m) =
      flip evalStateT 0
    . flip runReaderT mempty
    . flip evalStateT PSQ.empty
    . flip runContT (pure . pure)
    $ m

-- tester :: AStarT Int (Sum Int) Identity x
-- tester = do

    -- asum [spend 10 *> done 1, spend 10 *> done 2, done 3]
