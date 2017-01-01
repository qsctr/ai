# Informed search

See the source code on GitHub: https://github.com/qsctr/navigate

Use the program: https://qsctr.github.io/navigate (It is recommended to use the latest version of Chrome)

## 11/3/2016:

The goal of this project is to use uniform cost, greedy, and A* search algorithms to search a graph. But instead of searching a graph of cities in Romania I used the OpenStreetMap Overpass API to query the intersections of any roads near the user's location, and search the graph of the intersections, similar to a navigation program. However it does not take into account things like traffic lights or one-way roads.

I used TypeScript (and HTML and CSS) for this project. TypeScript is a superset of JavaScript which adds types to the language and compiles to JavaScript.

In the website, after the user selects an area to search, it queries the OpenStreetMap Overpass API to get all the points on all the roads in the area. It then creates a graph by turning the points into nodes and connecting them with each other. After that the user can choose a starting node and a goal node from the map, and also the type of search (uniform cost, greedy, or A*). After the search is finished, the amount of nodes expanded, the path found, and the fringe are shown.

Uniform cost, greedy, and A* searches all work in a similar way. The graph is searched by assigning each node a score and expanding the nodes in order of ascending scores. They are only different in the way the score is calculated for each node. So I wrote a general search function called `bestFirstSearch`.

```typescript
function bestFirstSearch(start: GraphNode, goal: GraphNode,
scoreFunc: ScoringFunction): SearchResult {
    const expanded: GraphNode[] = [];
    const scores = new Map<Id, Score>([[start.id, scoreFunc({
        node: start,
        parentScore: 0,
        distance: 0
    })]]);
    const parents = new Map<Id, GraphNode>();
    const fringe = new PriorityQueue<GraphNode>({
        comparator: (a: GraphNode, b: GraphNode) => scores.get(a.id) - scores.get(b.id),
        initialValues: [start]
    });
    while (fringe.length > 0) {
        const current = fringe.dequeue();
        if (expanded.includes(current)) {
            continue;
        }
        expanded.push(current);
        const currentScore = scores.get(current.id) as Score;
        if (current === goal) {
            const path = [goal];
            let parent = goal;
            while (parent !== start) {
                parent = parents.get(parent.id) as GraphNode;
                path.unshift(parent);
            }
            return { expanded, path, fringe };
        }
        for (const { distance, child } of current.connections) {
            if (expanded.includes(child)) {
                continue;
            }
            const oldChildScore = scores.get(child.id);
            const newChildScore = scoreFunc({
                node: child,
                parentScore: currentScore,
                distance
            });
            if (oldChildScore === undefined || newChildScore < oldChildScore) {
                scores.set(child.id, newChildScore);
                parents.set(child.id, current);
                fringe.queue(child);
            }
        }
    }
    return { expanded };
}
```

Here are all the interface and type definitions for the types used in the function above.

```typescript
type Id = number;
type Score = number;
type Distance = number;

interface GraphNode {
    readonly lat: number;
    readonly lng: number;
    readonly id: Id;
    readonly connections: Connection[];
}

interface Connection { // "Adjacency" is too hard to spell
    readonly distance: Distance;
    readonly child: GraphNode;
}

type ScoringFunction = (info: {
    node: GraphNode;
    parentScore: Score;
    distance: Distance;
}) => Score;

type SearchFunction = (start: GraphNode, goal: GraphNode) => SearchResult;

type SearchResult = {
    expanded: GraphNode[],
    path?: GraphNode[],
    fringe?: PriorityQueue<GraphNode>
};
```

A priority queue is basically a data structure which is first in, smallest out. So unlike a normal queue, the order you put stuff into it doesn't matter. TypeScript/JavaScript doesn't have a built-in priority queue data type, so I used [a library](https://github.com/adamhooper/js-priority-queue/).

I used `Map`s for keeping track of the scores and parents of nodes instead of mutable properties on the node objects themselves, because I didn't like the idea of the nodes being mutated after going through a search and having to reset them. `Map` is a built-in JavaScript data type (since ECMAScript 2015).

`bestFirstSearch` has an argument called `scoreFunc`, which is the function to call to assign a score to each node.

In uniform cost search, the scoring function for each node is the total distance traveled from the start node to the current one. In other words, it is the sum of the parent score and the distance from the parent, if the score for the starting node is 0.

```typescript
const uniformCostSearch: SearchFunction = (start, goal) =>
    bestFirstSearch(start, goal, ({ parentScore, distance }) => parentScore + distance);
```

In greedy search, the scoring function for each node is the straight-line distance from that node to the goal node.

```typescript
const greedySearch: SearchFunction = (start, goal) =>
    bestFirstSearch(start, goal, ({ node }) => distanceBetween(node, goal));
```

`distanceBetween` is just the distance formula.

```typescript
function distanceBetween(a: GraphNode, b: GraphNode) {
    return Math.sqrt(Math.pow(a.lat - b.lat, 2) + Math.pow(a.lng - b.lng, 2));
}
```

And A* is a combination of uniform cost and greedy.

```typescript
const aStarSearch: SearchFunction = (start, goal) =>
    bestFirstSearch(start, goal, ({ node, parentScore, distance }) =>
        parentScore + distance + distanceBetween(node, goal));
```

Uniform cost search can find the shortest path. However, it also takes more time than the other searches, since it has to expand a lot of nodes.

Greedy search is less accurate than uniform cost, but it is very fast, because it expands very few nodes.

A* search is in the middle. It is faster than uniform cost, but still quite accurate.

For my project, even though there can sometimes be tens of thousands of nodes in total, searching is still very fast, so uniform cost search is the best option to choose in this case.