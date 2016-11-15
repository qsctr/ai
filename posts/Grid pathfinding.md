# Grid pathfinding

## 11/14/2016

The grid pathfinding project is when pathfinding algorithms such as uniform cost search, greedy search, and A* are applied to nodes in a grid. I did both square and hexagon grids. In a square grid, each square is connected to the 8 squares around it. In a hexagon grid, each hexagon is connected to the 6 hexagons around it.

There can also be obstacles in the grid, and the search algorithm has to find a path around the obstacles. With not very optimal searches such as greedy search, the obstacles can end up making the path much longer than the path found with uniform cost search.

In my program, the grid is infinite, so if the goal is completely surrounded by obstacles, the search will never end (unless the starting node is also surrounded by obstacles).

I liked this project because it let me apply the different pathfinding algorithms that I learned to a grid, so I could compare their performance more clearly than with a fixed set of data. I also learned how to implement the algorithms in a functional language like Haskell, since the previous implementation I did (see "Informed search") was in TypeScript and it was imperative.

```javascript
// TODO: Add code
```