# Guessing Game

See the source code on GitHub: https://github.com/qsctr/guessing-game

## 10/13/2016:

Note: the first part is an explanation of the Guessing game, you can scroll down until you see Haskell code if you already know what the Guessing game is.

Guessing game is a game where you think of something in some topic and the computer has to guess what you are thinking. But the computer is dumb, and it doesn't know many things. But it can learn new things when you guess something that it doesn't know.

When the game starts, it asks a yes or no question. Depending on whether you answer yes or no, it asks another question, and so on, until it thinks that it has narrowed it down enough to only one thing. It then asks if you are thinking of that thing. If you say yes, then great, the computer got it right. But if you say no, then the computer wants to learn that new thing that it doesn't know about. So first it asks you what it is. Then it asks you what makes it different from the thing that it already knows. So the next time you play, when it asks the same questions and gets to the same place, instead of asking if it is one thing, it asks another question, which is whether it matches the description of the new thing which it learned from you. If you answer yes, then it asks if it is the new thing. If you answer no, then it asks if it is the old thing. And then you can answer yes or no, and if you answer no, then it can learn another thing from you, and the game goes on like that.

This can be easily implemented (and also easily understood because the above paragraph was probably extremely confusing) using binary trees. If you don't know what a tree is, a tree is a data structure where there is a node, and the node has several children, which are other nodes. And those nodes have more children, and so on. And there are some nodes at the very bottom of the tree which don't have children, or else the tree would go on forever. And all the nodes, except the one at the very top (called the root node), have a parent. And a binary tree is a tree where the nodes have at most 2 children. They can each have 0, 1, or 2 child nodes.

In the guessing game, each node can have a question associated with it, and either 2 or 0 children. If it has 2 children, then the first child is the "no" node, and the other one is the "yes" node. When the question is asked, if the user answers "no" to the question, the question belonging to the "no" child node is asked. If the user answers "yes" then the question belonging to the "yes" child node is asked. And it goes on like that. But a node can also have no children. In that case it would be a specific thing in the topic. For example in the topic of "blogs", it could be "Bretton's AI Blog". Even though binary trees can have 1 child, it doesn't make sense to do that in the Guessing Game.

Here is a diagram of a binary tree.

```
     a
   /   \
  b     c
 / \   / \
d   e f   g
```

For each node, the left side corresponds to "no" and the right side is "yes". Let's say the computer asks "a" which is a question. The user answers "yes". The computer asks "c". The user answers "no". The computer asks whether the user is thinking of "f" which is a thing. The user says "no". The user says that he/she is thinking of "h" and what makes it different from "f" is that "h" is different in some way, which we will call "i". The computer then makes a new tree, which looks like this.

```
     a
   /   \
  b     c
 / \   / \
d   e i   g
     / \
    f   h
```

Now "i" which is a question replaces "f", because there are two possible things which both fit the description of "c", and there has to be another description to distinguish between them. So when the user gets here again, the computer asks "i". And "yes" goes to "h" and "no" goes to "f". And the user can play again, and the tree keeps growing.

I did this project using the Haskell programming language. In Haskell it is very easy to define your own custom data type. Each type can have one or more value constructors. And those constructors can take any number of arguments.

Here is how I initially defined a (binary) tree.

```haskell
data Tree = Node String Tree Tree | Nil
```

This is saying, a `Tree` can be either `Nil` or a `Node` with a `String` and two sub-`Tree`s. Note that this is a recursive data structure since `Tree`s can have other `Tree`s in them (this is allowed when defining data types in Haskell).

So if I wanted a `Node` with no child nodes I would write `Node "hello" Nil Nil`. That would be a `Node` called "hello" with no child nodes. And if I wanted a `Node` with two child nodes I would write `Node "parent" (Node "child 1" Nil Nil) (Node "child 2" Nil Nil)`. And I could replace those `Nil`s with even more `Nodes` to create a bigger tree.

However, with this definition it is possible to create a `Node` with only one child: `Node "bad node" Nil (Node "child" Nil Nil)`. You don't want this to happen in the Guessing Game because there should only be nodes with 0 or 2 children. So I changed the definition to be like this.

```haskell
data Tree = Branch String Tree Tree | Leaf String deriving (Show, Read)
```

This way a `Tree` is either a `Branch` with a question and two sub-trees or a `Leaf` with a name (ignore the `deriving` stuff first). There is no way to create a node with one child. (Note that `Branch` and `Leaf` are both nodes, I just named them `Branch` and `Leaf` to distinguish between them.) (There is also no way to create a tree with absolutely nothing (just `Nil` in the previous definition) but there's no point in doing that anyways.)

Now there is the issue of writing/reading a `Tree` to/from a file. But luckily Haskell has the `Show` and `Read` typeclasses. You can think of typeclasses as similar to interfaces in object-oriented programming, where they specify some kind of behavior. When you make a type an instance of some typeclass, you can use the type with certain functions. For example, types which are instances of the `Eq` typeclass can be compared using the `(==)` function. So types which are instances of the `Show` and `Read` typeclasses can be used with the `show` and `read` functions respectively. The `show` function converts a type into a `String`, similar to `toString` in some languages. The `read` function is the inverse of `show`, it takes a `String` and converts it to a specific type. When you make new types, Haskell lets you derive instances for certain typeclasses for those types, so they automatically can be used with those typeclasses. So when I added `deriving (Show, Read)` to the end of the data declaration, `Tree`s can now be used with `show` and `read`, so they can be converted to and from `String`s, so they can be easily saved to/from a file.

Here is the `yesOrNo` function. It takes 3 arguments, the thing to ask the user, what to do if the user says "yes", and what to do if the user says "no". If the user says anything else, then it uses recursion and calls itself again until the user says either "yes" or "no".

```haskell
yesOrNo :: String -> IO a -> IO a -> IO a
yesOrNo question y n = putStrLn (question ++ "? (Y/N)") >> map toUpper <$> getLine >>= yn
  where yn "Y" = y
        yn "N" = n
        yn _ = putStrLn "You can only answer Y/N" >> yesOrNo question y n
```

Now here is the `main` function. It has a sub-function `run` which uses recursion to let the user play again. Note that it uses the `read` and `show` functions.

```haskell
main :: IO ()
main = putStrLn "Enter file to read" >> getLine >>= run
  where run file = do
          putStrLn "Think of a thing that is in the topic, then press enter" >> getLine
          readFile file >>= play . (read :: String -> Tree) >>= writeFile file . show
          yesOrNo "Play again" (run file) (return ())
```

And finally here is the `play` function, which takes a `Tree` and returns a new and improved `Tree` with a thing that it learned from the user added in (that is, if the computer failed to guess what the user was thinking). It uses pattern matching to match against either a `Branch` or a `Leaf`. If it is a `Branch` it calls itself again, but this time giving the sub-tree corresponding to the user's yes/no answer as an argument, and using the result of that as the new sub-tree. If it is a `Leaf` then it either returns itself if the computer guessed right, or a new `Branch` and two leaves if it guessed wrong. And when all the functions return up to the "top" one, the result is a new complete tree.

```haskell
play :: Tree -> IO Tree
play (Branch b n y) = yesOrNo b (Branch b n <$> play y) (flip (Branch b) y <$> play n)
play (Leaf l) = yesOrNo ("Is it " ++ l) (return (Leaf l)) $ do
  newLeaf <- putStrLn "What is it?" >> getLine
  newBranch <- putStrLn ("What makes it different from " ++ l ++ "?") >> getLine
  return (Branch newBranch (Leaf l) (Leaf newLeaf))
```

That is my whole program. I liked this project because of two reasons. First, I got to use trees in Haskell which was very fun, and it is much more concise than the same thing in other languages like Java. Second, we don't have to use graphics anymore.