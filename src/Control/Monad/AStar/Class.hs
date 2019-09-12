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

class (MonadPlus m, Monoid c) => MonadAStar c r m | m -> r, m -> c where
  -- | ADD to your current branch's CUMULATIVE cost. May cause a branch switch.
  spend :: c -> m ()

  -- | SET the current branch's BEST-CASE-COST cost. May cause a branch switch.
  estimate :: c -> m ()

  ---- | Perform a spend and estimate the heuristic at the same time
  ----
  ---- If you need to do both 'spend' and 'estimate' this is usually more efficient.
  ---- Every update to the branches cost causes expensive branch switching.
  --estimateAndSpend :: c  -- ^ Amount to 'estimate'
  --                 -> c -- ^ Amount to 'spend'
  --                 -> m ()
  --estimateAndSpend est spnd = estimate est >> spend spnd

  -- | Return a solution and short-circuit any remaining branches.
  done :: r -> m a

-- | Branch the search.
--
-- > branch == (<|>)
branch :: MonadAStar c r m => m a -> m a -> m a
branch = (<|>)

-- | Fail the current branch.
--
-- > branch == empty
failure :: MonadAStar c r m => m a
failure = empty
