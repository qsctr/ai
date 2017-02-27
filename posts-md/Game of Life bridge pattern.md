# Game of Life bridge pattern

## 2/27/2017

For this project we had to use the bridge pattern in one of our projects. I decided to use the game of life project, but I wrote it in Haskell, so I couldn't use the bridge pattern. So I wrote another one in TypeScript.

The bridge pattern is a pattern in OOP that makes it easy to switch implementations that are used by abstractions. There is an interface that specifies the methods for the implementations, and classes that implement that interface. Then, there is an abstract class which contains a protected member of the type of that interface, so it can hold any implementation. The classes which use that abstraction extend the abstract class. They initialize the implementation instance variable to be a certain implementation. The implementation can be changed later by simply assigning another implementation to the instance variable.

In this project I had a `Graphics` interface. I had two classes which implemented the interface, called `Console` and `Canvas`. `Console` draws onto the console, and `Canvas` starts a web server on localhost and opens it and sends and recieves data to/from it using websockets. The web page then displays the graphics using HTML5 canvas. There is also a `Simulation` class which has a protected `Graphics` member and a class `GameOfLife` which `extends Simulation` which contains the logic for the game of life.