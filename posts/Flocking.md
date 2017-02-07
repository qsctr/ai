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

I also defined some vector-related methods that I would use.

```python
    # still in class Vector

    @property
    def magnitude(self):
        return hypot(*self)

    def normalize(self):
        return self / self.magnitude if self.magnitude else self

    def limit(self, n):
        return self.normalize() * n if self.magnitude ** 2 > n ** 2 else self
```

The `@property` is a decorator for the magnitude function that turns it into a property. So instead of calling `.magnitude()` you can just access `.magnitude` which would invoke the function.

Finally, I added a function to average vectors.

```python
# outside of class Vector

def average(vectors):
    return sum(vectors) / len(vectors) if vectors else Vector()
```

Then, I created the `Boid` class.

```python
from math import atan2, degrees
from vector import average, Vector

class Boid:
```

First, I put a lot of properties in the class. I started out just using them as constants and not intending to change them, but later I added controls to change these values, which would affect all boids since they are static.

```python
    # still in class Boid

    max_speed = 3
    max_force = 0.03

    separation_radius = 100
    neighbor_radius = 200

    separation_weight = 1
    alignment_weight = 1
    cohesion_weight = 1

    speed_multiplier = 1

    width = 40
    height = 20
    color = 255, 255, 255
```

The ~~constructor~~ initializer is pretty straightforward.

```python
    # still in class Boid
    
    def __init__(self, position, velocity=(0, 0)):
    self.position = Vector(position)
    self.velocity = Vector(velocity)
```

Then I added some useful methods for boids.

```python
    # still in class Boid
    
    @property
    def angle(self):
        return degrees(atan2(*reversed(self.velocity.coordinates)))

    def distance(self, other):
        return (other.position - self.position).magnitude
```

Here are the three behaviors.

```python
    # still in class Boid
    
    def separation(self, others):
        return average([(self.position - other.position).normalize() / self.distance(other)
                        for other in others if self.distance(other) < self.separation_radius])

    def alignment(self, others):
        return average([other.velocity for other in others
                        if self.distance(other) < self.neighbor_radius]).limit(self.max_force)

    def cohesion(self, others):
        return average([other.position - self.position for other in others
                        if self.distance(other) < self.neighbor_radius]).limit(self.max_force)
```

And here is the method to actually update the boid.

```python
    # still in class Boid

    def update(self, flock):
        others = list(filter(lambda boid: boid is not self, flock))
        acceleration = (self.separation(others) * self.separation_weight +
                        self.alignment(others) * self.alignment_weight +
                        self.cohesion(others) * self.cohesion_weight)
        velocity = (self.velocity + acceleration).limit(self.max_speed) * self.speed_multiplier
        position = self.position + self.velocity
        return Boid(position, velocity)
```

Notice that it returns a new `Boid`, instead of updating the current one. I actually at first wrote it so that the boids are mutable and they would be modified. But then, when going through the flock of boids to update each one, I wanted them to update simultaneously in each step. In other words, if there were 3 boids A, B, and C, when A updates, it uses the B and C values from the previous frame. But when B updates, it uses the A value from the next frame and the C value from the previous frame. And when C updates, it uses the A and B values from the next frame. So their behavior would depend on the order of updating. What I needed to do was make a copy of each boid so they could be updated without replacing their previous values. So I simply returned a new `Boid` in the `update` method each time. So each frame, the flock is replaced with a new flock of new boids.

```python
# TODO: finish this post
```
