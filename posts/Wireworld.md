# Wireworld

See the source code on GitHub: https://github.com/qsctr/wireworld

Wikipedia article on Wireworld: https://en.wikipedia.org/wiki/Wireworld

## 8/21/2016:

Since Wireworld is very similar to the Game of Life, to begin with I copied the Game of Life code and changed everywhere where it said "Game of Life" to "Wireworld".

In Wireworld, each cell has 2 pieces of information: its location, and its type. In the Game of Life each alive cell only had a location, all alive cells were the same type. So, I changed the definition of `World` so that `cells` was is a list of pairs, with the first item being the `CellType` and the second being the location, which was another pair of `Int`s.

```haskell
data World = World { cells :: [(CellType, (Int, Int))], playing :: Bool, viewPort :: ViewPort }
```

`CellType` is defined as

```haskell
data CellType = EHead | ETail | Conductor deriving (Eq)
```

There is no need for the `Empty` type because then the cell wouldn't be in the list of alive cells in the first place.

For the `drawCell` function in `drawWorld`, the cell is drawn with different colors depending on its type.

```haskell
drawCell (cellType, (x, y)) = color c $ translate (fromIntegral x) (fromIntegral y) $ rectangleSolid 1 1
    where c = case cellType of
              EHead -> blue
              ETail -> red
              Conductor -> yellow
```

Because there are different rules for Wireworld, the code for `stepWorld` (when the game isn't paused) is different.

```haskell
stepWorld _ world@World {..} = world { cells = map stepCell cells }
    where stepCell (EHead, pos) = (ETail, pos)
          stepCell (ETail, pos) = (Conductor, pos)
          stepCell (Conductor, (x, y))
              | surroundingEHeadsCount `elem` [1, 2] = (EHead, (x, y))
              | otherwise = (Conductor, (x, y))
              where surroundingEHeadsCount = length $ getEHeadsAt [ (x + w, y + h) | w <- [-1..1], h <- [-1..1] ]
          getEHeadsAt = foldr (\pos acc -> case find ((== pos) . snd) cells of
              Just cell -> if fst cell == EHead then cell : acc else acc
              Nothing -> acc) []
```

Because no dead cells become alive and vice versa, I no longer have to check if each surrounding cell should be alive or dead. However I do have to check the new type of each cell.

In `handleInput`, the code for the mouse button events is different. Now, when the left button is pressed, it switches between alive and dead, and when the right button is pressed, it changes the type of the cell.

```haskell
handleInput (EventKey (MouseButton button) Down _ coords) world@World {..}
    | button == LeftButton = case findCell of
        Just cell -> world { cells = delete cell cells }
        Nothing -> world { cells = (Conductor, pos) : cells }
    | button == RightButton = case findCell of
        Just cell -> world { cells = (nextType $ fst cell, pos) : delete cell cells }
        Nothing -> world
    where findCell = find ((== pos) . snd) cells
          pos = mapPair round $ invertViewPort viewPort coords
          nextType EHead = ETail
          nextType ETail = Conductor
          nextType Conductor = EHead
```

I also changed the background to be black instead of white.

That is all of the changes. Most of the code is still the same as Game of Life. It has the same functionality, so the grid is infinite, the viewport is zoomable and movable, etc.