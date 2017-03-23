# Perceptron

## 3/23/2017

This is the first project about machine learning. Perceptron is basically a function which takes some inputs and returns an output. The inputs are multiplied with weights and the sum is checked against a threshold. The goal of the Perceptron is to learn the weights and threshold from a training set of data.

For this project the task of the Perceptron was to divide two sets of points on a plane. So the Perceptron takes two inputs, the x and y coordinates of the point. The Perceptron can be graphed as a line on the plane, with the threshold divided by the x weight as the x intercept, and the threshold divided by the y weight as the y intercept. The goal is to make an animation of the Perceptron so that you can see it learning the weights and fitting the data in real time.

I did this project in the Haskell programming language. I defined a data type called `Perceptron` to hold information related to the Perceptron.

```haskell
data Perceptron = Perceptron { weights :: [Float], threshold :: Float, rate :: Float }
```

Here is the activate function for the Perceptron.

```haskell
activate :: Perceptron -> [Float] -> Float
activate Perceptron {..} = fromIntegral . fromEnum . (> threshold) . sum . zipWith (*) weights
```

However, I also wrote a logistic function, which makes the Perceptron appear smoother in the animation, and also makes it divide the points at the center instead of stopping once it works correctly.

```haskell
activate Perceptron {..} = (1 /) . succ . exp . negate . subtract threshold . sum . zipWith (*) weights
```

And here is the learning function.

```haskell
learn :: ([Float], Int) -> Perceptron -> Perceptron
learn (inputs, target) Perceptron {..} = Perceptron
    { weights = zipWith learnWeight weights inputs
    , threshold = learnWeight threshold (-1)
    , .. }
    where
    learnWeight weight input =
        weight + rate * (fromIntegral target - activate Perceptron {..} inputs) * input
```

As you can see, the threshold can be treated as a weight with the input always at -1.

Even though in this project I am only using two inputs, the Perceptron functions here can handle any number of inputs.