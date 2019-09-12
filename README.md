A\* Monad
=========

[Hackage](http://hackage.haskell.org/package/astar-monad)

**Caveat Emptor**; this hasn't been *battle-tested* yet; it should work, but make sure to test it out if you're doing anything serious.

Easily do [A\* searches](https://en.wikipedia.org/wiki/A*_search_algorithm) with use of arbitrary monadic effects!

> A* Search is widely used in pathfinding and graph traversal, which is the process of finding a path between multiple points, called "nodes".
> A* can be used to find optimal paths for any problem with an appropriate heuristic cost-measure.

## Basics

* Use `<|>` or `asum` (anything using `Alternative`) to branch into multiple possible choices.
* Use `estimate myCost` to **set** the value of your 'heuristic' function for the current branch. Do this whenever you've done enough work to change your estimate.  Remember that A\* heuristics should always be pessimistic (e.g. can over-estimate cost, but shouldn't UNDER estimate). 
* Use `spend myCost` to **add** a cost to the branch's CUMULATIVE cost. Do this when some cost has been incurred, e.g. we've moved the state from one node to another.
* Every call to `spend` or `estimate` creates a branch; Branches with LOWER costs will run before those with higher costs. Note that this branching is expensive, so try to consolidate calls to these functions in your actions when possible.
* Call `done mySolution` to short circuit ALL running branches and immediately return your result.
* `AStarT` has a built-in State monad which can store branch-local states for you. You can store your current branch's solution-space for instance, or the path you've followed to get to the current solution; or both!

Here's an example of using A\* to find a path to a location in a 2 dimensional grid.

```haskell
-- Track which moves we've made, up, down, left or right
data Move = U | D | L | R
    deriving (Show, Eq)

-- Track our current position, the goal we're moving towards, and the locations we've crossed along the way.
data Context =
    Context { _current   :: (Int, Int)
            , _goal      :: (Int, Int)
            , _locations :: [(Int, Int)]
            }
    deriving (Show, Eq)
makeLenses ''Context

-- The Manhattan distance between two points serves as our heuristic estimate.
-- It's *conservative*; it may cost MORE than this, but it won't cost LESS
distanceTo :: (Int, Int) -> (Int, Int) -> Int
distanceTo (x, y) (x', y') = abs (x - x') + abs (y - y')

-- To make things more interesting we'll say that moving through coordinates
-- where either 'x' or 'y' are divisible by the other is 3X more expensive!
addCost :: MonadAStar (Sum Int) r m => (Int, Int) -> m ()
-- Don't divide by zero!
addCost (0, _) = spend 1
addCost (_, 0) = spend 1
addCost (x, y)
    | mod x y == 0 || mod y x == 0 = spend 3
addCost _ = spend 1

-- Move around the space looking for the destination point.
mineField :: AStar Context (Sum Int) (Int, Int) ()
mineField = do
    -- Get the current position
    (x, y) <- use current
    -- Add the current location to our list
    locations <>= [(x, y)]
    -- If we're at the goal we're done!
    gl <- use goal
    when ((x, y) == gl)
      $ done gl
    -- Add the cost of the current position
    addCost (x, y)
    -- Estimate steps to completion
    estimate . Sum $ distanceTo gl (x, y)
    asum
        [ move L >> mineField
        , move R >> mineField
        , move U >> mineField
        , move D >> mineField
        ]

-- We only care about the ending state, so we use `execAStar`
-- `runAStarT` is the most powerful and runs a monad-transformer version
-- and returns both the state and result type.
run :: Maybe Context
run = execAStar findPoint
             Context { _current = (5, 5)
                     , _goal    = (7, 4)
                     , _moves   = []
                     }

-- Execute it to see if we found a solution; 
-- We only care about the state, so we use 'execAStar' it returns the state of the the 'winning' branch.
--
-- We can see from the results that the optimal path with these costs involves a bit of moving around to avoid coordinates which divide each other!
>>> execAStar mineField (context (1, 1) (3, 4))
Just (Context
      { _current   = (3, 4)
      , _goal      = (3, 4)
      , _locations = 
          [(1, 1), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (1, 5), (2, 5), (3, 5), (3, 4)]
      })

```
