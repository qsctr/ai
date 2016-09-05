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

![A two wheeled vehicle](images/vehicle.png)

## 9/6/2016

Yesterday I finished the project. I originally planned to do more things, but I also had lots of other homework this week, so I didn't do a lot of extra stuff.

I spent a long time thinking about how a vehicle should be represented. In the end I decided to represent a vehicle as a function, because if you think about it, you continuously put information in (where the source is) and continuously get information out (the new vehicle location and angle). However, in order for the function to be referentially transparent, i.e. determines its outputs from only its inputs, you need to pass the time that has passed, so it knows how much to advance, and the previous vehicle location and angle.

```haskell
type VehicleFunction = SourceLocation -> TimePassed -> VehicleState -> VehicleState
```

And here is `VehicleState`.

```haskell
data VehicleState = VehicleState { location :: Point, angle :: Float }
```

However, the `VehicleFunction` only specifies the type of vehicle. In other words, it only has the rules, and not the actual information. Here is a complete `Vehicle`.

```haskell
data Vehicle = Vehicle { function :: VehicleFunction, picture :: Picture, state :: VehicleState }
```

The `function` field is the rule of the vehicle, the `picture` field is a picture of it, and the `state` field contains its state.

For this project, we are supposed to make each vehicle have `sense`, `decide`, and `act` commands, to be like an agent. So, I created the `makeVehicle` function, which, when passed three functions (`sense`, `decide`, `act`), and a `Picture`, returns a `Vehicle`.

```haskell
makeVehicle
    :: (SourceLocation -> VehicleState -> sensorValues)                 -- sense
    -> (sensorValues -> motorVelocities)                                -- decide
    -> (TimePassed -> VehicleState -> motorVelocities -> VehicleState)  -- act
    -> Picture
    -> Vehicle
makeVehicle sense decide act pic =
    let vFunc sourceLoc t vSt = limitAct sourceLoc t vSt $ decide $ sense sourceLoc vSt
        limitAct sourceLoc t vSt v =
            let newVSt = act t vSt v
            in  if distance (location newVSt) (location vSt) < 0.03
                    && distance sourceLoc (location vSt) < 58
                then vSt else newVSt
        defaultState = VehicleState { location = (0, 0), angle = 0 }
    in  Vehicle vFunc pic defaultState
```

It combines the `sense`, `decide`, and `act` commands to create the `function`, then puts that together with a default state and the supplied picture.

Here is the `sensorRule` function. It is applied to the distance between the source and the sensor, to get the sensor value.

```haskell
sensorRule :: Distance -> SensorValue
sensorRule x
    | x < mouseCircleRadius = (1 / x ^ 2) * 1e6
    | otherwise = 0
```

The rule is to apply the inverse square law, then multiply by 1e6.

Since many vehicle share the same `sense` and `act` commands, I decided to separate them into different modules which can be imported by the vehicles that need them. So, I created the modules `Vehicles.OneWheeled` and `Vehicles.TwoWheeled`. (There is only one one-wheeled vehicle, but I wanted it to be consistent.)

Here are the important functions and values in `OneWheeled`.

```haskell
vehiclePicture :: Picture
vehiclePicture = pictures
    [ rectangleWire width 20
    , translate (-30) 0 wheel
    , translate (width / 2) sensorPositionY sensor ]

sense :: SourceLocation -> VehicleState -> SensorValue
sense sourceLoc vSt = applySensorRule sourceLoc vSt sensorPosition

act :: TimePassed -> VehicleState -> MotorVelocity -> VehicleState
act t (VehicleState { location, angle }) v =
    VehicleState { location = moveAtAngle angle (v * t) location, angle }
```

This code is pretty simple, since the vehicle only has one sensor and moves in a straight line.

Here are the important functions from `TwoWheeled`.

```haskell
vehiclePicture :: Picture
vehiclePicture = pictures
    [ rectangleWire width 40
    , uncurry translate lWheelPosition wheel
    , uncurry translate rWheelPosition wheel
    , translate (width / 2) sensorPositionsY sensor
    , translate (width / 2) (-sensorPositionsY) sensor ]

sense :: SourceLocation -> VehicleState -> (SensorValue, SensorValue)
sense sourceLoc vSt = mapPair (applySensorRule sourceLoc vSt) sensorPositions

act :: TimePassed -> VehicleState -> (MotorVelocity, MotorVelocity) -> VehicleState
act t (VehicleState { location, angle }) (vl, vr) =
    let (dl, dr) = (vl * t, vr * t)
        moveWheel d wheelPos = moveAtAngle angle d $ addPoints location $ rotatePoint angle wheelPos
        (newLeftLoc, newRightLoc) = (moveWheel dl lWheelPosition, moveWheel dr rWheelPosition)
        newLocation = subtractPoints (midpoint newLeftLoc newRightLoc) $
            rotatePoint newAngle (wheelPositionsX, 0)
        newAngle = addAngles angle $ atan' $ (dr - dl) / (wheelPositionsY * 2)
    in  VehicleState { location = newLocation, angle = newAngle }
```

The `act` function is a lot more complicated, which I won't explain here but you can understand by reading the code.

You might notice that there are a lot of math functions such as `moveAtAngle`. They are defined in the module `Vehicles.Math`.

```haskell
-- you already saw this before
sensorRule :: Distance -> SensorValue
sensorRule x
    | x < mouseCircleRadius = (1 / x ^ 2) * 1e6 -- inverse square law
    | otherwise = 0

inverse :: SensorValue -> MotorVelocity
inverse 0 = 0
inverse x = 1e4 / x -- this is to prevent the motor power from being too small for inversed vehicles

distance :: Point -> Point -> Distance
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

midpoint :: Point -> Point -> Point
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

moveAtAngle :: Angle -> Distance -> Point -> Point
moveAtAngle angle d (x, y) = (x + cos' angle * d, y + sin' angle * d)

rotatePoint :: Angle -> Point -> Point
rotatePoint angle (x, y) =
    moveAtAngle (addAngles angle $ atan' $ y / x) (distance (0, 0) (x, y)) (0, 0)

addAngles :: Angle -> Angle -> Angle
addAngles a b =
    let wrap x
            | x > 360 = wrap $ x - 360
            | x < 0 = wrap $ x + 360
            | otherwise = x
    in  wrap $ a + b

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subtractPoints :: Point -> Point -> Point
subtractPoints (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

-- The built-in trigonometry functions in Haskell use radians, so I have to convert the values.
sin', cos', atan' :: Float -> Float
sin' = sin . degToRad
cos' = cos . degToRad
atan' = radToDeg . atan
```

Also, here is the `applySensorRule` function which was used in both one-wheeled and two-wheeled vehicles.

```haskell
applySensorRule :: SourceLocation -> VehicleState -> SensorPosition -> SensorValue
applySensorRule sourceLoc (VehicleState { location, angle }) =
    sensorRule . distance sourceLoc . addPoints location . rotatePoint angle
```

You might notice that instead of having three arguments, like the function signature implies, only two arguments are written. This is because the right side of the = sign is a function, which is a composition of many functions. This works because in Haskell functions are curried.

And here is the module `Vehicle.Parts`, which defines some common parts for the pictures of the vehicles.

```haskell
wheel :: Picture
wheel = rectangleWire 24 12

sensor :: Picture
sensor = pictures
    [ line [(0, 0), (sensorLineLength, 0)]
    , translate sensorOffset 0 $ arc 90 270 sensorRadius ]

sensorLineLength, sensorRadius, sensorOffset :: Float
sensorLineLength = 20
sensorRadius = 6
sensorOffset = sensorLineLength + sensorRadius
```

With all these functions defined, the actual definitions for the vehicles are quite simple.

For `vehicle1` the `sense` and `act` are from the module `Vehicles.OneWheeled`.

```haskell
vehicle1 :: Vehicle
vehicle1 =
    let decide = id
    in  makeVehicle sense decide act vehiclePicture
```

For the rest of the vehicles they all use `Vehicles.TwoWheeled`.

```haskell
vehicle2a :: Vehicle
vehicle2a =
    let decide = id
    in  makeVehicle sense decide act vehiclePicture

vehicle2b :: Vehicle
vehicle2b =
    let decide = swap
    in  makeVehicle sense decide act vehiclePicture

vehicle2c :: Vehicle
vehicle2c =
    let decide (l, r) = let avg = (l + r) / 2 in (avg, avg)
    in  makeVehicle sense decide act vehiclePicture

vehicle3a :: Vehicle
vehicle3a =
    let decide =  mapPair inverse
    in  makeVehicle sense decide act vehiclePicture

vehicle3b :: Vehicle
vehicle3b =
    let decide = swap . mapPair inverse
    in  makeVehicle sense decide act vehiclePicture
```

The vehicles vary only in their `decide` functions.

The `Vehicles.Vehicles` function keeps a list of the vehicles and their respective names, for the user to type into the console.

```haskell
allVehicles :: [(VehicleName, Vehicle)]
allVehicles =
    [ ("1", vehicle1)
    , ("2a", vehicle2a)
    , ("2b", vehicle2b)
    , ("2c", vehicle2c)
    , ("3a", vehicle3a)
    , ("3b", vehicle3b) ]
```

And finally we get to the `Main` module.

The `main` function is quite long, because it randomizes the states of each vehicle.

```haskell
main :: IO ()
main = do
    mapM_ putStrLn
        [ "Enter a vehicle."
        , "You can also enter multiple vehicles separated by spaces."
        , "The vehicles can be the same types."
        , "Available vehicle types are: " ++ unwords (map fst allVehicles) ]
    input <- words <$> getLine
    case mapM lookupVehicle input of
        Left err -> putStrLn err >> main
        Right vehicles -> do
            let (w, h) = (800, 600)
            initialState <- do
                let randomize vehicle = do
                        let randomAround x = randomRIO (- x / 2, x / 2)
                        randX <- randomAround $ fromIntegral w - 100
                        randY <- randomAround $ fromIntegral h - 100
                        let location = (randX, randY)
                        angle <- randomRIO (0, 359)
                        return $ vehicle { state = VehicleState { location, angle } }
                randomVehicles <- mapM randomize vehicles
                return $ State { vehicles = randomVehicles, mouse = Nothing }
            mapM_ putStrLn 
                [ "Instructions:"
                , "Space bar: toggle mouse control"
                , "Press enter to continue." ]
            _ <- getLine
            play (InWindow "Vehicles" (w, h) (0, 0)) white 60 initialState draw handleInput update
```

Here are the other functions. They are quite simple to read.

```haskell
draw :: State -> Picture
draw (State { vehicles, mouse }) =
    let mousePicture = case mouse of
            Just (x, y) -> translate x y $ circle mouseCircleRadius
            Nothing -> blank
        drawVehicle (Vehicle { picture, state = VehicleState { location = (x, y), angle } }) =
            translate x y $ rotate (-angle) $ picture
    in  pictures $ mousePicture : map drawVehicle vehicles

update :: TimePassed -> State -> State
update _ state@(State { mouse = Nothing }) = state
update time state@(State { mouse = Just sourceLoc, vehicles }) =
    let updateVehicle vehicle@(Vehicle { function, state = vState }) =
            vehicle { state = function sourceLoc time vState }
    in  state { vehicles = map updateVehicle vehicles }

handleInput :: Event -> State -> State
handleInput (EventKey (SpecialKey KeySpace) Down _ pos) state@(State { mouse }) =
    case mouse of
        Nothing -> state { mouse = Just pos }
        Just _ -> state { mouse = Nothing }
handleInput (EventMotion pos) state@(State { mouse }) =
    case mouse of
        Nothing -> state
        Just _ -> state { mouse = Just pos }
handleInput _ state = state
```

An important factor when I designed this structure for the code was extensibility. New vehicles can be easily added on, simply by defining the `sense`, `decide`, and `act` functions, and a picture. Or it only needs to define `decide` if it uses modules like `Vehicles.TwoWheeled`. Then, the vehicle needs to be included with its name in `allVehicles`.

One problem I faced was that vehicle 3a, which is supposed to go towards a source, and stay at the source, slowed down, but then went past the source. This was due to the steps being discrete, instead of ideally being continuous, even though it is running at 60 steps per second. The vehicle advanced step by step, and eventually, even though the steps were getting smaller, still stepped over the source, then sped up again.

I solved this by forcing the vehicle to stay in the same place if it is really close to the source and the distance that it traveled was very small. It is in the `makeVehicle` function above.

I actually added support for adding multiple sources last night, like a source is created when you click somewhere, but it didn't work and I didn't want to debug it because I was really tired last night and it was past midnight, so I just reverted the code.

Some future improvements I could make are to add support for multiple sources, and add vehicle 4a and 4b.

I will upload the program later because I don't have time now.

Braitenberg vehicles are very interesting. I like how they move differently according to different rules. They remind me of the electric field hockey ~~game~~ simulation we ~~played~~ used in physics class last year. I want to read Braitenberg's complete book one day.