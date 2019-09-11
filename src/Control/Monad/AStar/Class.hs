{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.AStar.Class (MonadAStar(..), branch, failure) where
import Control.Monad
import Control.Applicative

-- | A class which represents the ability to do A* search.
--
-- The laws aren't completely pinned down yet, but these should probably hold:
--
-- > It should short-circuit on 'done'
-- > done a >> mx == done a
-- > done a <|> mx == done a
-- >
-- > It should fail a branch using `empty`.
-- > empty >> mx == empty
-- > empty <|> mx == mx
-- >
-- > It should branch respecting costs using `<|>` from its 'Alternative' instance.
-- > (updateCost 2 >> mx) <|> (updateCost 1 >> my) == mx <|> my

class (MonadPlus m) => MonadAStar w r m | m -> r, m -> w where
  -- | Update the cost estimate of the current branch and re-evaluate available branches,
  -- switching to a cheaper one when appropriate.
  updateCost :: w -> m ()

  -- | Return a solution and short-circuit any remaining branches.
  done :: r -> m a

-- | Branch the search.
--
-- > branch == (<|>)
branch :: MonadAStar w r m => m a -> m a -> m a
branch = (<|>)

-- | Fail the current branch.
--
-- > branch == empty
failure :: MonadAStar w r m => m a
failure = empty
