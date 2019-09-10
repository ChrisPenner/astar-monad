{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.AStar.Class (MonadAStar(..)) where

-- | A class which represents the ability to do A* search.
class MonadAStar w r m | m -> r, m -> w where
  -- | Branch the search
  branch :: m a -> m a -> m a

  -- | Update the cost estimate of the current branch and re-evaluate available branches,
  -- switching to a cheaper one when appropriate.
  updateCost :: w -> m ()

  -- | Return a solution and short-circuit any remaining branches.
  done :: r -> m a
