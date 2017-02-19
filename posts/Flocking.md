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

## 2/12/2017

I made several changes to the code. I will talk about those first before explaining the `flocking` module.

First, I moved the `angle` method to the `Vector` class, so I could get the angles of vectors other than Boid velocities.

```python
    # inside class Vector
    
    @property
    def angle(self):
        return degrees(atan2(*reversed(self.coordinates)))
```

Also, I improved the `limit` method.

```python
    # still in class Vector
    
    def limit(self, n):
        return self.normalize() * n if self.magnitude > n else self
```

I modified some of the boid properties to make the simulation look better.

```python
    # inside class Boid
    
    max_speed = 4
    max_force = 0.05

    separation_radius = 100
    neighbor_radius = 200

    separation_weight = 5
    alignment_weight = 1
    cohesion_weight = 1

    speed_multiplier = 1

    width = 40
    height = 20
    color = 255, 255, 255
```

I added mouse following to the boids. Since I just added it, it is probably still a little glitchy but it looks okay.

```python
    # still in class Boid
    
    def mouse(self, mouse_pos):
    if mouse_pos is None:
        return Vector()
    difference = Vector(mouse_pos) - self.position
    return difference.normalize() / max(difference.magnitude, 100)
```

And finally I updated `update`. It now takes the mouse position as a parameter to do the mouse following.

```python
    # still in class Boid
    
    def update(self, flock, mouse_pos):
    others = list(filter(lambda boid: boid is not self, flock))
    velocity = (self.velocity +
                self.separation(others) * self.separation_weight +
                self.alignment(others) * self.alignment_weight +
                self.cohesion(others) * self.cohesion_weight +
                self.mouse(mouse_pos) * 20  # TODO: use configurable weight
                ).limit(self.max_speed) * self.speed_multiplier
    position = self.position + velocity
    return Boid(position, velocity)
```

Now I will explain the `flocking` module which is the main module to run the simulation.

```python
from boid3 import Boid
from os import system
import pygame
from random import randrange
from vector import average
```

Here are some constants.

```python
window_width = 800
window_height = 600
background_color = 0, 0, 0
icon_color = 255, 255, 255
icon_size = 256
fps = 30
```

Here are some global variables that will be used.

```python
flock = []
running = True
mouse_pos = None
mouse_on = False
```

Here is the code that is needed to initialize pygame.

```python
pygame.init()
screen = pygame.display.set_mode((window_width, window_height))
pygame.display.set_caption('Flocking')
clock = pygame.time.Clock()
pygame.key.set_repeat(500, 100)
icon = pygame.Surface((icon_size, icon_size), pygame.SRCALPHA).convert_alpha()
```

This function is run every frame, to handle events.

```python
def check_events():
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            return False
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 3:  # right click
                global mouse_on
                mouse_on = not mouse_on
                update_console()
            elif empty(event.pos):
                flock.append(Boid(event.pos))
                update_console()
        elif event.type == pygame.MOUSEMOTION:
            global mouse_pos
            mouse_pos = event.pos
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
                global running
                running = not running
                update_console()
            elif event.key == pygame.K_RETURN:
                for i in range(100):
                    pos = randrange(window_width), randrange(window_height)
                    if empty(pos):
                        flock.append(Boid(pos))
                        update_console()
                        break
            elif event.key == pygame.K_BACKSPACE and flock:
                flock.pop()
                update_console()
            elif event.key in keys:
                attr, diff = keys[event.key]
                prev = getattr(Boid, attr)
                if event.mod in [pygame.KMOD_LSHIFT, pygame.KMOD_RSHIFT]:
                    setattr(Boid, attr, prev - diff)
                else:
                    setattr(Boid, attr, prev + diff)
                update_console()
    return True
```

It returns whether the program should continue. In other words, if it returns `False`, the program should end.

This function checks if a given point is empty or not.

```python
def empty(pos):
    return all(boid.position.coordinates != pos for boid in flock)
```

These are the keys that adjust the boid properties. The `check_events` function handles the actual updating, this is just a dictionary mapping from a key (literally) to the property name and how much it should change by. The default values of the properties are what they start as in the `Boid` class.

```python
keys = {
    pygame.K_1: ('max_speed', 0.5),
    pygame.K_2: ('max_force', 0.01),
    pygame.K_3: ('separation_radius', 20),
    pygame.K_4: ('neighbor_radius', 20),
    pygame.K_5: ('separation_weight', 0.2),
    pygame.K_6: ('alignment_weight', 0.2),
    pygame.K_7: ('cohesion_weight', 0.2),
    pygame.K_8: ('speed_multiplier', 0.2),
    pygame.K_9: ('width', 5),
    pygame.K_0: ('height', 5)
}
```

This function updates the console. It is very straightforward.

```python
def update_console():
    system('cls')  # clears the console (only works on Windows)
    print('Flocking')
    print()
    print('Left click to add a boid at the mouse pointer')
    print('Right click to toggle follow mouse')
    print('Press enter to add a boid at a random location')
    print('Press backspace to delete the last boid')
    print('Press space to pause/continue')
    print('Press the number to increase the corresponding value')
    print('Hold shift to decrease')
    print()
    for key, (name, _) in keys.items():
        print(f'[{pygame.key.name(key)}] {name} = {getattr(Boid, name):.2f}')
    print()
    print(f'Running: {running}')
    print(f'Follow mouse: {mouse_on}')
    print(f'Boid count: {len(flock)}')
```

The strings with `f` before them are (appropriately) called f-strings. They were introduced in Python 3.6 to do string interpolation. Basically, whatever is inside the `{}` is evaluated as an expression and then joined with the rest of the string. So `f'1 + 1 = {1 + 1}.'` would evaluate to `'1 + 1 = 2.'`.

Here is a function to draw boids.

```python
def draw_boid(boid):
    surface = pygame.Surface((boid.width, boid.height), pygame.SRCALPHA).convert_alpha()
    pygame.draw.polygon(surface, boid.color,
                        [(0, 0), (boid.width, boid.height / 2), (0, boid.height)])
    rotated = pygame.transform.rotate(surface, -boid.velocity.angle)
    center = boid.position - (boid.width / 2, boid.height / 2)
    screen.blit(rotated, center.coordinates)
```

First the boid is drawn onto a square transparent surface. Then, the surface is rotated, and the rotated surface is blitted (drawn) onto the screen at the boid's coordinates. I could probably optimize this by not making a new surface for every single boid in every single frame, since the un-rotated surface is always the same given that `Boid.width` and `Boid.height` stay the same.

This is a function which updates the icon. I made it so that the application icon is a tiny boid, and it would face in the same direction as the average directions of the boids in the simulation. So when the boids turn, the icon would turn too. Unfortuately, after the program runs for several minutes, the icon freezes and stops changing (on Windows at least). I don't know why this is happening. The code is quite similar to the `draw_boid` function above.

```python
def update_icon():
    if flock:
        avg_angle = average([boid.velocity for boid in flock]).angle
        longer = icon_size
        shorter = icon_size * (Boid.height / Boid.width)
        if Boid.width > boid.height:
            width = longer
            height = shorter
        else:
            width = shorter
            height = longer
        surface = pygame.Surface((icon_size, icon_size), pygame.SRCALPHA).convert_alpha()
        pygame.draw.polygon(surface, (255, 255, 255), [
            (icon_size / 2 - width / 2, icon_size / 2 - height / 2),
            (icon_size / 2 + width / 2, 128),
            (icon_size / 2 - width / 2, icon_size / 2 + height / 2)
        ])
        rotated = pygame.transform.rotate(surface, -avg_angle)
        pygame.transform.scale(rotated, (icon_size, icon_size), icon)
    else:
        icon.fill((0, 0, 0, 0))
    pygame.display.set_icon(icon)
```

Now here is the main loop. But first, the console needs to be updated.

```python
update_console()

while check_events():
    if running:
        screen.fill(background_color)
        flock = [boid.update(flock, mouse_pos if mouse_on else None) for boid in flock]
        for boid in flock:
            boid.position %= window_width, window_height
            draw_boid(boid)
        pygame.display.update()
        update_icon()
    clock.tick(fps)
```

You can see that I am updating the flock all at once, instead of each boid individually. Then I am using `%` on the boid's coordinates with the screen dimensions to wrap them around the edge of the screen.
