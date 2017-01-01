# Vacuum world

See the source code on GitHub: https://github.com/qsctr/vacuum-world

## 9/29/2016:

In Vacuum world there are two connected rooms. Each one can either have dirt in it or not, and there is a vacuum cleaner in one room.

![Vacuum world](images/vacuum-world.png)

The job of the vacuum cleaner is to go around and suck all the dirt in the least steps possible. The number of steps is also known as the cost. The possible actions are:

![Vacuum actions](images/vacuum-actions.png)

Since the configuration is very simple, there are only 8 possible states. So it is possible to explicitly specify the sequence of actions the vacuum should perform for each of the 8 states. However I decided to make the vacuum decide the next step based on the current state.

I used Haskell for the project again. One cool thing about Haskell is that it is easy to define custom data types. So I defined three data types.

```haskell
data Vacuum = L | R deriving (Eq, Show)
data Action = MoveL | MoveR | Suck | NoOp deriving (Eq, Show)
data State = State
    { vacuum :: Vacuum
    , dirtL :: Bool
    , dirtR :: Bool
    , action :: Action
    , playing :: Bool
    , cost :: Int
    , vacuumPicture :: Picture
    , dirtPicture :: Picture
    } deriving (Show)
```

So for example, `Vacuum` is the name of the type. And `L` and `R` are the value constructors for that type (note that this is different than object-oriented constructors). For the last data type, there is only one constructor, called `State`, but many states are possible, since the constructor takes named arguments such as the vacuum position, whether there is dirt in either room, etc.

Here is the `update` function. It is called at each step to update the `State`.

```haskell
update :: State -> IO State
update state@State {..}
    | playing = printState $ act { cost = cost + (if decide == NoOp then 0 else 1), action = decide }
    | otherwise = return state
    where   sense = vacuum == L && dirtL || vacuum == R && dirtR
            decide
                | sense = Suck
                | vacuum == L = if dirtR then MoveR else NoOp
                | vacuum == R = if dirtL then MoveL else NoOp
            act = case decide of
                MoveR -> state { vacuum = R }
                MoveL -> state { vacuum = L }
                Suck -> if vacuum == L then state { dirtL = False } else state { dirtR = False }
                NoOp -> state
```

This function is very straightforward. It creates 3 values depending on the state, `sense`, `decide`, and `act`. `sense` is a `Bool` representing whether there is dirt in same room as the vacuum. `decide` is an `Action` which depends on the result of `sense` and the current state. `act` is a new `State` which is based off the old state and depends on the result of `decide`. And finally the function returns the state of `act` but with the `cost` increased by 1 and the `action` changed to the new action from `decide`. Actually the only purpose of the `cost` and `action` fields is to display the information to the user, since the state is printed out every time it is updated. For displaying information I am actually just printing the `State` data structure directly.

I liked doing this project. Some future improvements I could make are to let the vacuum clean more than two rooms, arranged in different ways, or to make integer amounts of dirt rather than just a boolean amount.