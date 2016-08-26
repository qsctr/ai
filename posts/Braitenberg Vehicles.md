# Braitenberg Vehicles

See the source code on GitHub: https://github.com/qsctr/vehicles

## 8/26/2016:

Today I made a picture of a vehicle with two wheels.

```haskell
vehicle :: Picture
vehicle = scale 2 2 $ pictures
    [ rectangleWire 30 20
    , translate (-12) (-13) $ wheel
    , translate (-12) 13 $ wheel
    , translate 0 6 $ sensor
    , translate 0 (-6) $ sensor ]
    where wheel = rectangleWire 12 6
          sensor = translate 15 0 $ pictures
              [ line [(0, 0), (10, 0)]
              , translate 13 0 $ arc 90 270 3 ]
```

The result looks like this.

![A two wheeled vehicle](https://github.com/qsctr/ai/blob/gh-pages/images/vehicle.png?raw=true)
