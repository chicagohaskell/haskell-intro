---
title: Haskell for JavaScript hackers
---

https://github.com/chicagohaskell/haskell-intro

Tooling
====

Core programs
---

- `ghc` - dominant Haskell compiler
- `cabal` - package management and build system
- `stack` - omnibus tool to manage a Haskell project (we'll use this)

Installation
---

```shell
$ curl -sSL https://get.haskellstack.org/ | sh
```

... or use your distro package manager: 

```shell
$ apt install haskell-stack
$ brew install haskell-stack
$ pacman -S stack
```

... then

```shell
$ stack upgrade
```

---

test that everything is working:

```shell
$ git clone https://github.com/chicagohaskell/haskell-intro.git
$ cd haskell-intro
$ stack build
```

Editor setup
---

See `editor_setup.md` for details about how to further customize your editor.

Resources
---

- https://hackage.haskell.org - one stop shop for Haskell libraries
- `#haskell` on Freenode - quirky but generally helpful and friendly crowd
- https://www.haskell.org/documentation - many free books on Haskell
- https://www.haskell.org/hoogle - search engine for terms by type

The language
====

---

+--------------+-------------------------+--------------------+
|              | JavaScript              | Haskell            |
+==============+=========================+====================+
| emphasis     | expressiveness          | correctness        |
+--------------+-------------------------+--------------------+
| evaluation   | eager                   | lazy               |
+--------------+-------------------------+--------------------+
| paradigm     | imperative              | declarative        |
+--------------+-------------------------+--------------------+
| memory mgmt  | gc                      | gc                 |
+--------------+-------------------------+--------------------+

---

+--------------+-------------------------+--------------------+
|              | JavaScript              | Haskell            |
+==============+=========================+====================+
| typings      | dynamic                 | strong; inferred   |
+--------------+-------------------------+--------------------+
| qa           | exhaustive unit testing | type safety + unit |
+--------------+-------------------------+--------------------+
| produces     | scripts                 | native code        |
+--------------+-------------------------+--------------------+
| syntax       | C family                | Lisp family        |
+--------------+-------------------------+--------------------+


---

```haskell
main = putStrLn "Hello Chicago!"

reverse = inner []
  where
  inner xs' [] = xs'
  inner xs' (x:xs) = inner (x:xs') xs
```

```javascript
console.log("Hello Chicago!");

function reverse(xs) {
  const result = [];
  for (let x of xs) {
    result.unshift(x);
  }
  return result;
}
```

Type system
----

- every expression _must_ have a type 
- types are deduced from the terms in scope
- algebraic data types 
  ```haskell
  -- String OR (Int AND Int)
  data Task
    = Print String
    | Add Int Int  

  data Box = Box { width :: Int, height :: Int }

  -- pattern match on the constructor
  runTask (Print msg) = putStrLn msg
  runTask (Add x y) = print $ x + y
  ```

Polymorphism
----

```haskell
-- inferred above
reverse :: [a] -> [a]
```
::: incremental
- we don't look at the list items so who cares what type they are?
- sometimes called generics
- adds expressiveness to Haskell
:::

---

functions with heavy daily use tend to be very polymorphic:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
flip :: (a -> b -> c) -> (b -> a -> c)
```

functions like these are _absolutely neccessary_ to do functional programming

Type classes
----

kinship with _interfaces_ but more general

```haskell
-- overload names, like "show"
formatValue :: Show a => a -> String
formatValue x = "x = " ++ show x

-- composable code generation 
type Api 
  = "menu" 
  :> Capture "item-id" String 
  :> Get '[JSON] MenuItem

getter :: String -> ClientM MenuItem
getter = client (Proxy @Api) 
```

---

Syntax for writing type classes

```haskell
class IntRep a where
  toInt :: a -> Int 

instance IntRep Bool where
  toInt x = if x then 1 else 0
```

IO
----

- side effects and values from the outside world are modeled using a special 
  container `IO`.  
- `IO` is like `Promise` but lazy.
- `do` notation is analogous to `async/await`

---

JavaScript version:

```javascript
async function mediate(params) {
  x = await getFromApi1(params);
  await pushToApi2({
    userAgent: "chicago-haskell",
    value: x
  });
  await saveToDisk({
    timestamp: Date.now(),
    params,
    value: x
  });
}
```

---

```haskell
getFromApi1 :: Params -> IO Value
pushToApi2 :: String -> Value -> IO ()
saveToDisk :: UTCTime -> Params -> Value -> IO ()
```

```haskell
-- syntactically like synchronous JS
-- the types work like Promise
mediate :: Params -> IO ()
mediate p = do
  x <- getFromApi1 p
  pushToApi2 "chicago-haskell" x
  t <- getCurrentTime
  saveToDisk t p x
```

---

... is syntactic sugar for

```haskell
-- without do notation
-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- >>= is analogous to ".then"
mediate' p = getFromApi1 p >>= \x ->
  pushToApi2 "chicago-haskell" x >>
  getCurrentTime >>= \t ->
  saveToDisk t p x
```

   

Control flow
----

Haskell is declarative but supports sequential computation.  

```haskell
main = loop 1
  where
  loop n = do
    putStrLn $ "try #" ++ show n
    firstName <- getLine
    lastName <- getLine
    let name = firstName ++ " " ++ lastName
    putStrLn $ "Is " ++ name ++ " correct?"
    conf <- getLine
    if conf == "y" 
      then putStrLn $ "Hello " ++ name ++ "!"
      -- control is often handled by recursion
      else loop (n+1)
```

---

there are many useful combinators for expressing control flow in the standard library

```haskell
import Control.Monad (forM, unless)
main = do
  nums <- fmap (read @Int) . words <$> getLine
  unless (length nums == 0) $ 
    -- nums.forEach(n => { console.log(`another number ${n}`); });
    forM nums $ \n ->
      putStrLn $ "another number: " ++ show n
```

Higher kinded types
----

a type can be an abstract shape in a strong sense

```haskell
-- Standard list type
data List a
  = EmptyList
  | Cons a (List a)

exampleList = Cons 1 . Cons 2 . Cons 3 $ EmptyList
--- The built in list type has syntactic sugar for this
--- [1, 2, 3] = 1 : 2 : 3 : []
```

---

example: a tree whose left leaves are one type and whose right leaves are another

```haskell
data FancyTree a b 
  = Leaf (Either a b) -- data Either a b = Left a | Right b
  | Branch (LTree a b) (RTree a b)

data LTree a b = LLeaf a | LBranch (LTree a b) (RTree a b)
data RTree a b = RLeaf b | RBranch (LTree a b) (RTree a b)

exampleTree = 
  (LLeaf "hello" `LBranch` RLeaf 7) `Branch` RLeaf 22
```


---

operations on containers can be overloaded wrt the container

```haskell
-- Functors are essentially containers that 
-- can be mapped over
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap xs

negList = fmap negate [1..5]
-- > [-1, -2, -3, -4, -5]
```

---

interesting things can be containers

```haskell
newtype AssocList a b = AssocList [(a, b)]

instance Functor (AssocList a) where
  fmap _ (AssocList []) = AssocList []
  fmap f (AssocList (x,y):xys) = 
    let AssocList xys' = fmap f $ AssocList xys in
    AssocList (x, f y) : xys'
```

this allows you to map over the `b` part of an `AssocList a b`

Laziness
----

Haskell does not evaluate expressions unless they are needed for a subsequent 
computation (ghc might even trim them from the binary)

```haskell
-- the runtime memoizes the part of this 
-- list that actually gets computed 
fib = 0 : 1 : zipWith (+) fib (tail fib)

-- take 6 fib
-- > [0, 1, 1, 2, 3, 5]

-- this prints hello and exits
f _ = print "hello"
main = f [0..] 
```

Persistent data structures
----

- values (even complex) are immutable
  ```haskell
  x :: [Int]
  x = [1, 5]
  -- ... almost anything happens here
  -- x is still [1, 5] 
  ```
- in the lazy evaluation model, operations on persistent data can be efficient (on average)



