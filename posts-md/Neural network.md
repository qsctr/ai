# Neural network

See the source code on GitHub: https://github.com/qsctr/neural-network, https://github.com/qsctr/neural-network-classify

## 5/19/2017

For my semester 2 final project I decided to write my own feed-forward neural network in Haskell. I think the hardest part was writing the backpropagation algorithm. Here is the code that trains the neural network.

```haskell
train :: ActivationFns -> Double -> TrainingSample -> Network -> (Network, Double)
train (g, g') r (is, ts) (checkNet -> net) = (snd $ bProp is net oss, err)
  where err = sum [ (t - o)^2 / 2 | t <- ts | o <- fst $ last oss ]
        oss = fProp is net
        fProp _ [] = []
        fProp is' (l:ls) = (os, map g' nis) : fProp os ls
          where nis = netInputs is' l
                os = map g nis
        bProp is' [l] [(os, os')] = (ds, [update is' l ds])
          where ds = [ o' * (t - o) | o <- os | o' <- os' | t <- ts ]
        bProp is' (cl:nl:ls) ((os, os'):oss') = (cds, update is' cl cds : net')
          where (pds, net') = bProp os (nl:ls) oss'
                cds = [ o' * sum [ ws !! i * pd | (ws, _) <- nl | pd <- pds ]
                    | o' <- os' | i <- [0..] ]
        bProp _ _ _ = undefined
        update is' l ds = [ (zipWith wUpdate ws is', wUpdate b 1)
            | (ws, b) <- l | d <- ds, let wUpdate w i = w + r * i * d ]
```

I also modified my perceptron project to use a neural network, so there can be multiple lines on the screen, and it can classify non-linear inputs such as xor.
