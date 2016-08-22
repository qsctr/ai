# Game of Life

See the source code on GitHub: https://github.com/qsctr/game-of-life

## 8/17/2016:

I decided to make the Conway's Game of Life simulation using the Haskell programming language. Haskell is a purely functional programming langauge. For graphics I used a Haskell library called Gloss.

I had never used external Haskell libraries before, so I spend the first few days trying to figure out how to set everything up. In the end I found out I had to use a tool called stack to manage dependencies and build my project.

## 8/19/2016:

I finished the project today. The game has the following features:

- Play the Game
- Pause
- Add cells
- Remove cells
- Navigate around
- Zoom in and out
- Reset zoom and navigation
- Restart game

Gloss has the `play` function for displaying graphics.

```haskell
play (InWindow "Game of Life" (800, 600) (0, 0)) white 5 initialWorld drawWorld handleInput stepWorld
```

The first arguments are all information about how the graphics should display. But the last 4 are variables or functions that are defined elsewhere. The functions are called with some data of any type. In this case I defined my own data type called `World`.

```haskell
data World = World { cells :: [(Int, Int)], playing :: Bool, viewPort :: ViewPort }
```

The `World` data type is a record type which contains 3 fields. Instead of the cells being a 2D matrix, I am storing a list of coordinates, so the "grid" can be (theoretically) infinitely big. Because Haskell is a purely functional language, I cannot store state in global variables. Instead I have to pass them around in a data type which gets passed to and returned by functions.

`initialWorld` is self-explanatory. It is passed to `play` as the starting state.

```haskell
initialWorld :: World
initialWorld = World { cells = [], playing = False, viewPort = initialViewPort }
```

`drawWorld` is a function which gets called by `play` to convert the game state into a `Picture`. The code is pretty self-explanatory too.

```haskell
drawWorld :: World -> Picture
drawWorld World {..} = applyViewPortToPicture viewPort $ pictures $ map drawCell cells
    where drawCell (x, y) = translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 1 1
```

`stepWorld` is a function which is passed a game state and returns a new game state.

```haskell
stepWorld :: Float -> World -> World
stepWorld _ world@World { playing = False } = world
stepWorld _ world@World {..} = world { cells = nub $ concatMap (filter alive . surrounding) cells }
    where surrounding (x, y) = [ (x + w, y + h) | w <- [-1..1], h <- [-1..1] ]
          alive cell = if neighbors == 4 then cell `elem` cells else neighbors == 3
              where neighbors = length $ filter (`elem` cells) $ surrounding cell
```

If `playing` is `False` then the world stays the same. Otherwise, for each cell that is existing in the list, a function is mapped to it which obtains the surrounding cells as a list, and returns the ones that should be alive in the next generation. This is because the only cells that could be alive are the ones that are surrounding existing alive cells. I cannot go through the entire grid because the grid is infinite in my implementation. Then, the list of lists of new cells is flattened, and duplicates are removed, and it is returned by `stepWorld`.

This function is quite inefficient, since the lists in Haskell are actually linked lists, which for certain operations take a very long time. One way that I could improve this is by skipping some cells which are unnecessary to be checked. Also, Haskell has a Set data type which is more efficient, and automatically does not allow duplicates. Switching to that should improve performance.

Finally, the `handleInput` function is passed an input event, for example a key press, and the current state, and it returns a new state.

```haskell
handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world@World {..} = world { playing = not playing }
handleInput (EventKey (MouseButton button) Down _ coords) world@World {..}
    | button == LeftButton && cell `notElem` cells = world { cells = cell : cells }
    | button == RightButton && cell `elem` cells = world { cells = delete cell cells }
    where cell = mapPair round $ invertViewPort viewPort coords
handleInput (EventKey (SpecialKey key) Down _ _) world@World {..}
    | key == KeyLeft = t (tx + 5, ty)
    | key == KeyRight = t (tx - 5, ty)
    | key == KeyUp = t (tx, ty - 5)
    | key == KeyDown = t (tx, ty + 5)
    where t translation = world { viewPort = viewPort { viewPortTranslate = translation } }
          (tx, ty) = viewPortTranslate viewPort
handleInput (EventKey (Char c) Down _ _) world@World {..}
    | c == 'z' = s $ zoom + 2
    | c == 'x' = s $ max 1 $ zoom - 2
    where s factor = world { viewPort = viewPort { viewPortScale = factor } }
          zoom = viewPortScale viewPort
handleInput (EventKey (Char 'c') Down _ _) world = world { viewPort = initialViewPort }
handleInput (EventKey (Char 'r') Down _ _) _ = initialWorld
handleInput _ world = world
```

The single function handles all of the features that respond to input such as adding and removing cells, zooming, etc.

So, that is pretty much the whole program. I think this graphics library is quite easy to use, so I will probably use it in the future if I am doing graphics with Haskell again. I enjoyed writing this program in Haskell, since it makes me think differently than when I am writing Java. Besides learning things about programming and graphics, I also learned that playing the Game of Life is very fun and I will probably play it in the future when I am bored. I am surprised at how the cells can seem to have complicated patterns while following very simple rules.

You can download the program on the GitHub Releases page: https://github.com/qsctr/game-of-life/releases

## 8/22/2016: (actually it's 8/23 at 12:05 am)

I changed `cells` from a linked list to a `Set`. Set is a data type which is ordered and cannot have duplicates. The program can now run way faster without lagging.

```haskell
data World = World { cells :: Set (Int, Int), playing :: Bool, viewPort :: ViewPort }
```

You can see the exact things that I changed in the commit diff.

https://github.com/qsctr/game-of-life/commit/995356ce07d57f712cf79d8563f498391f05c9d3

You can download the (way faster) program on the GitHub Releases page: https://github.com/qsctr/game-of-life/releases