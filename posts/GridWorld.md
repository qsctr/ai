# GridWorld

See the source code on GitHub: https://github.com/qsctr/gridworld

## 9/19/2016:

GridWorld is a simulation where there are walls and agents in a grid. Each agent follows the walls according to certain rules, so they are simple reflex agents. There can also be treasures in the world, and an agent will stop at a treasure if it finds one. I did my project using Haskell and the Gloss graphics library.

Here are the rules for GridWorld. At each step, each agent senses the eight blocks surrounding it in the grid, and whether they are occupied by walls or not. From those eight values, it then determines a "feature vector", which in this case happens to only have four values. It is determined by going clockwise around the surrounding squares starting from the top one, and combining every two squares into one. The way that they are combined is using the or operation, in other words, if either one is occupied, the feature vector treats both blocks as occupied. Then, the agent decides on its next move based on the feature vector. You can see how it decides in the code below.

Here is how I wrote the rules in my program. I separated them into `sense`, `decide`, and `act` functions.

```haskell
type SensorInput   = [Bool]
type FeatureVector = [Bool]
type Location      = (Int, Int)
type Action        = (Int, Int)
```

```haskell
sense :: SensorInput -> FeatureVector
sense = map or . chunksOf 2

decide :: FeatureVector -> Action
decide [True, True, True, True] = (0, 0)  -- don't move
decide [True, False, _, _]      = (1, 0)  -- east
decide [_, True, False, _]      = (0, -1) -- south
decide [_, _, True, False]      = (-1, 0) -- west
decide _                        = (0, 1)  -- north

act :: Location -> Action -> Location
act (x, y) (a, b) = (x + a, y + b)
```

The `type` keywords simply creates a synonym for another type to make the code easier to read. So `SensorInput` and `FeatureVector` are both actually just lists of `Bool`s. (Yes, `FeatureVector` isn't a `Vector`.)

The functions are pretty straightforward and easy to understand, except possibly `sense`. `sense` is defined as a composition of the `map or` function and the `chunksOf 2` function. Since function composition goes from right to left, the input is first split into chunks of 2 (so it is a list of length-2 lists), then the `or` function is applied to each element in the list, and you get a list as the output, since the `or` function takes a list and returns whether any of the values inside are `True`. For the `decide` function, the last equation has `_` as the argument, so by default, for any combination of values in the feature vector not mentioned in the above equations, the agent moves north. For example, if it is in empty space, it keeps moving north until it finds a wall to follow.

As with my previous AI projects, I used a record data structure to store the state of the program. Since Haskell does not allow global state, because if functions rely on the values of global variables then they will not be referentially transparent, I have to pass the state to all functions which rely on some information in the state, so that their output is only determined from their input.

```haskell
import qualified Data.Set as S
```

```haskell
import Graphics.Gloss.Data.ViewState
```

```haskell
type Locations = S.Set Location
```

```haskell
data Mode = Viewing | EditingWalls | EditingAgents | EditingTreasures deriving (Eq, Enum, Bounded)
```

```haskell
data State = State
    { walls       :: Locations
    , agents      :: Locations
    , treasures   :: Locations
    , playing     :: Bool
    , viewState   :: ViewState
    , windowW     :: Int
    , windowH     :: Int
    , mode        :: Mode
    , leftButton  :: Bool
    , rightButton :: Bool }
```

Like the Game of Life project, instead of using a matrix of values, I used three sets of `Location`s (I defined `Location` as a synonym for `(Int, Int)`) to represent the locations of the different objects in the simulation. This allows the grid to be infinitely large. `ViewState` is a data type from the `Graphics.Gloss.Data.ViewState` module which maps input events to viewport changes. And `Mode` is a data type that I defined which can have four values (see above).

All the other code in the program is for the additional features that I added, such as adding and removing objects, reading from and saving to text files, moving around and zooming the window, fitting the world to the screen, etc.

I think the most challenging functions to write in this program were the `parseMap`, `convertToText`, and `fitWindow` functions.

`parseMap` converts from a text file of a grid into the actual `State` data structure used by the program. Because I used sets of ordered pairs instead of a matrix to represent the objects in the world, the conversion process was complicated. I decided to stick with using an actual grid as the format for text files, because that would make it easier to edit the file by hand, comparing to editing a bunch of pairs separated by commas. The function also has to identify all the errors in the text file, if there are any, and combine them into a readable string, including the line numbers of each error in the text file.

```haskell
parseMap :: String -> Either ErrorMessage State
parseMap = fmap toState . combine . map (foldr add start . pair) . zip [0, -1 ..] . lines
    where
        start = Right (S.empty, S.empty, S.empty)
        pair (y, xs) = map (\ x -> ((x, y), xs !! x)) [0 .. length xs - 1]
        add ((x, y), c) (Right (wls, ags, trs))
            | toLower c == wallChar     = Right (S.insert (x, y) wls, ags, trs)
            | toLower c == agentChar    = Right (wls, S.insert (x, y) ags, trs)
            | toLower c == treasureChar = Right (wls, ags, S.insert (x, y) trs)
            | toLower c == emptyChar    = Right (wls, ags, trs)
            | otherwise                 = Left $
                "Illegal character '" ++ [c] ++ "' at line " ++ show (-y + 1) ++ ", col " ++ show (x + 1)
        add a (Left msg)
            | Left newMsg <- add a start = Left $ msg ++ "\n" ++ newMsg
            | otherwise                  = Left msg
        combine xs
            | any isLeft xs = Left $ intercalate "\n" $ lefts xs
            | otherwise     = fmap unzip3 $ sequence xs
        toState (wls, ags, trs) = initialState
            { walls     = S.unions wls
            , agents    = S.unions ags
            , treasures = S.unions trs }
```

The `convertToText` function is the inverse of `parseMap`. It converts a `State` with sets of pairs into a `String` which is a grid.

```haskell
convertToText :: State -> String
convertToText state@(State { walls, agents, treasures }) = unlines $ S.foldr (add treasureChar)
    (S.foldr (add agentChar) (S.foldr (add wallChar) emptyGrid walls) agents) treasures
    where
        add c = \ (x, y) acc ->
            let accx = acc !! (yMax - y)
            in  take (yMax - y) acc ++ [take (x - xMin) accx ++ [c] ++ drop (x - xMin + 1) accx]
                ++ drop (yMax - y + 1) acc
        emptyGrid = replicate (yMax - yMin + 1) $ replicate (xMax - xMin + 1) emptyChar
        ((xMax, xMin), (yMax, yMin)) = maxMins state
```

And the `fitWindow` function fits all objects currently in the world into the window, with as much zoom as possible. It was kind of annoying to write because it involved a lot of calculations.

```haskell
fitWindow :: State -> State
fitWindow state@(State { viewState, windowW, windowH }) = state
    { viewState = viewState
        { viewStateViewPort = (viewStateViewPort viewState)
            { viewPortScale = min (fromIntegral windowW / fromIntegral (xMax - xMin + 1))
                (fromIntegral windowH / fromIntegral (yMax - yMin + 1))
            , viewPortTranslate = (fromIntegral (-(xMax + xMin)) / 2 , fromIntegral (-(yMax + yMin)) / 2) } } }
    where
        ((xMax, xMin), (yMax, yMin)) = maxMins state
```

And here is the `maxMins` function which is used by both `convertToText` and `fitWindow`.

```haskell
maxMins :: State -> ((Int, Int), (Int, Int))
maxMins (State { walls, agents, treasures }) = ((S.findMax xs, S.findMin xs), (S.findMax ys, S.findMin ys))
    where
        everything = S.unions [walls, agents, treasures]
        xs         = S.map fst $ everything
        ys         = S.map snd $ everything
```

One thing that I discovered while working on this project is that my program can be used to create ASCII art, since you can draw walls and stuff and generate a text file from it. However the ASCII art would only contain `x`, `o`, and `$`, since those are the characters which represent walls, agents, and treasures.

In conclusion, I liked working on this GridWorld project a lot. But I spent a lot of time on it, so I didn't have time to do the optional TileWorld project.

This post only contains a small part of the code, so you should go to [GitHub](https://github.com/qsctr/gridworld/blob/master/src/Main.hs) to read the full program.