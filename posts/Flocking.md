# Flocking

See the source code on GitHub: https://github.com/qsctr/flocking

## 2/6/2017

Flocking is a simulation where there are boids in flocks. The boids move around and simulate flocking behavior in real life with things like birds and fish. Boid is short for bird-oid, but they are just generic objects that interact with other boids to form flocks. Each boid follows three behaviors: separation, alignment, and cohesion.

- Separation: A boid does not want to be too close to other boids.
- Alignment: A boid wants to move in the same direction as the others.
- Cohesion: A boid wants to stay close to the other boids.

These behaviors produce velocity vectors representing the direction and speed the boid should go, and the vectors are added together to get the resultant vector, which is then added to the position of the boid to make it move. However, each boid does not take into account all other boids in the world, just the ones within a certain radius, like real life animals.

You might think that separation and cohesion cancel each other out, since their vectors would point in opposite directions. But actually, the radii used for separation and cohesion could be different, so there is a difference between "close" and "too close". Also, separation force increases the closer the boid is to some other boid, but cohesion has the same weight for all boids within the radius.

We were required to do this project in Python, using a graphics library called pygame.

First, I created a vector class, since I would be dealing with a lot of vectors.

```python
from math import hypot
from operator import add, mod, mul, sub, truediv

class Vector:

    def __init__(self, coordinates=(0, 0)):
        self.coordinates = tuple(coordinates)
```

I wrapped coordinates in a tuple constructor call to allow passing in non-tuple iterables into the constructor function.

I also made vectors themselves an iterable, because it would be nice to be able to iterate over each coordinate of the vector, like how you can iterate over each element in a tuple. So I just return the iterator for the `coordinates` tuple in the implementation for `__iter__`.

```python
    # still in class Vector
    def __iter__(self):
        return iter(self.coordinates)
```

I thought it would be useful to use the math operators for dealing with vectors, so I implemented the numeric operator methods. I wanted them to work with scalars (regular numbers) as well as other vectors and tuples, so I had to do different things depending on whether the other operand was iterable or not.

```python
    # still in class Vector
    def __add__(self, other):
        return Vector(map(add, self, other if hasattr(other, '__iter__') else (other, other)))
```

But then I realized that the code would be very repetitive for the other operations, so I defined a function to make those functions.

```python
# outside of class Vector
def apply(op):
    return lambda self, a: Vector(map(op, self, a if hasattr(a, '__iter__') else (a, a)))
```

```python
    # inside class Vector
    __add__ = apply(add)
    __radd__ = __add__
    __sub__ = apply(sub)
    __mul__ = apply(mul)
    __truediv__ = apply(truediv)
    __mod__ = apply(mod)
```

```python
# TODO: finish writing this post
```
