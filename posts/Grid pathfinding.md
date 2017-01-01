# Grid pathfinding

See the source code on GitHub: https://github.com/qsctr/grid-pathfinding

## 11/14/2016

The grid pathfinding project is when pathfinding algorithms such as uniform cost search, greedy search, and A* are applied to nodes in a grid. I did both square and hexagon grids. In a square grid, each square is connected to the 8 squares around it. In a hexagon grid, each hexagon is connected to the 6 hexagons around it.

There can also be obstacles in the grid, and the search algorithm has to find a path around the obstacles. With not very optimal searches such as greedy search, the obstacles can end up making the path much longer than the path found with uniform cost search.

In my program, the grid is infinite, so if the goal is completely surrounded by obstacles, the search will never end (unless the starting node is also surrounded by obstacles).

I liked this project because it let me apply the different pathfinding algorithms that I learned to a grid, so I could compare their performance more clearly than with a fixed set of data. I also learned how to implement the algorithms in a functional language like Haskell, since the previous implementation I did (see "Informed search") was in TypeScript and it was imperative.

Since this project was similar to the GridWorld project I re-used some code from it.

The graphics are displayed on a coordinate plane. So for the square grid it is simple, because each square is one integer coordinate. But for the hexagon grid I put the surrounding hexagons further away, so they would form a hexagon shape around the central one. If (0, 0) was a hexagon, the surrounding hexagons would be (0, 4), (3, 2), (3, -2), (0, -4), (-3, -2), and (-3, 2).

I used [this library](https://github.com/lspitzner/pqueue) for priority queues in Haskell.