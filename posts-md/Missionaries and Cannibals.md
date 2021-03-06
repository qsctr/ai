# Missionaries and Cannibals

See the source code on GitHub: https://github.com/qsctr/missionaries-and-cannibals

## 10/13/2016:

Missionaries and Cannibals is a problem where there is a river with a boat and two sides. There are 3 missionaries and 3 cannibals on the right side. The goal is to get all of them to the left side. The boat can hold two people, and one person has to row the boat for it to go across the river. However, whenever there are more cannibals than missionaries on a side, the cannibals eat the missionaries.

I wrote a program in Haskell which can find the shortest possible solution (least number of steps) to the problem. It can also try to find the steps between any starting state and any ending state, with any number of missionaries or cannibals or boat size. When a solution is found it can either print the steps or play an ASCII animation in the terminal. I clear lines and change colors using the `ansi-terminal` Haskell package, since ANSI escape codes are not natively supported in the win32 terminal.

There is a function which takes a state and returns a list of the possible next states. When the search starts it puts the starting state into a list. Then it gets all possible next states by using the bind (`>>=`) function to feed the list into the function, with the list monad. It keeps doing this until one of the states inside the list is the goal state. Then, it prints out the steps associated with the state.