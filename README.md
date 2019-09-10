A\* Monad
=========

[Hackage](http://hackage.haskell.org/package/astar-monad)

Easily do A\* searches with use of arbitrary monadic effects!

## Basics

* Use `<|>` or `asum` (anything using `Alternative`) to branch into multiple possible choices.
* Use `updateCost myCost` to set the value of your 'heuristic' function whenever you've done enough work to change your estimate.  Remember that A\* heuristics should always be pessimistic (e.g. can over-estimate cost, but shouldn't UNDER estimate). 
* Every call to `updateCost` creates a branch; Branches with LOWER costs will run before those with higher costs.
* Call `done mySolution` to short circuit ALL running branches and immediately return your result.
* `AStarT` has a built-in State monad  which **automatically keeps state contiguous in spite of branching**. This means that your state monad will properly switch states when switching branches. Just use state normally, it should work as expected. You can store your current branch's solution-space for instance, or the path you've followed to get to the current solution; or both!

Here's an example of using A\* to find a path to a location in a 2 dimensional grid.

```haskell
-- Track which moves we've made, up, down, left or right
data Move = U | D | L | R
    deriving (Show, Eq)

-- Track our current position, the goal we're moving towards, and the moves we've taken so far.
data Context =
    Context { _currentPos :: (Int, Int)
            , _goal    :: (Int, Int)
            , _moves   :: [Move]
            }
    deriving (Show, Eq)
makeLenses ''Context

-- The Manhattan distance between two points
-- This is our A* heuristic
distanceTo :: (Int, Int) -> (Int, Int) -> Int
distanceTo (x, y) (x', y') = abs (x - x') + abs (y - y')

-- Move around the space looking for the destination point.
findPoint :: AStar Context Int () ()
findPoint = do
    c <- use currentPos
    gl <- use goal
    -- I could return the moves we took, 
    -- but our State is automatically returned when we run AStar
    when (c == gl) $ done ()
    -- We have more work to do, we should update the cost estimate and continue
    updateCost $ distanceTo gl c
    if c == gl 
       then done ()
       else updateCost $ distanceTo gl c
    -- Non-deterministically choose a direction to move, 
    -- store that move in our state, and edit our current position.
    asum
        [ moves <>= [R] >> currentPos . _1 += 1 >> findPoint
        , moves <>= [L] >> currentPos . _1 -= 1 >> findPoint
        , moves <>= [D] >> currentPos . _2 += 1 >> findPoint
        , moves <>= [U] >> currentPos . _2 -= 1 >> findPoint
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

-- run it to see if we found a solution; it returns the state of the the 'winning' branch.
>>> run 
Just (Context { _current = (7, 4)
              , _goal    = (7, 4)
              , _moves   = [U, R, R]
              })
```
