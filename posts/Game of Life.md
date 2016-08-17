# Game of Life

See the source code on GitHub: https://github.com/qsctr/game-of-life

8/17/2016

I decided to make the Conway's Game of Life simulation using the Haskell programming language. Haskell is a purely functional programming langauge. For graphics I used a Haskell library called Gloss, which uses OpenGL internally.

I had never used external Haskell libraries before, so I spend the first few days trying to figure out how to set everything up.

I decided to use a list of coordinates to store the cells. This way the grid could be infinitely big.

```haskell
type Cell = (Int, Int)
```

(I will add more stuff later)