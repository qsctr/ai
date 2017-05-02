# Half moon perceptron

See the source code on GitHub: https://github.com/qsctr/perceptron-half-moon

## 4/24/2017

This project is pretty simple, it is just the perceptron project, but classifying two groups of inputs in half moon shapes. Therefore, I used pretty much the same code as the perceptron project. I added this piece of code that generates half moon shapes using polar coordinates.

```haskell
import Control.Monad
import System.Random

halfMoon :: Float -> Float -> IO [(Float, Float)]
halfMoon minAngle maxAngle = replicateM 1000 $ do
    r <- randomRIO (rMin, rMax) -- rMin and rMax are defined elsewhere
    θ <- randomRIO (minAngle, maxAngle)
    return (r * cos θ, r * sin θ)
```
