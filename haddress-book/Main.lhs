---
title: Address book
---

This simple program provides a console where the user can edit an address book 
file.

> import Data.Aeson as Ae
> import Data.Bool
> import qualified Data.ByteString.Lazy as BSL
> import Data.Char
> import Data.List
> import Data.Map.Strict as Map
> import System.Directory 

Normally we would get the location of the data file from a command line option, 
but for now let's hard-code it. 

> dataFile = "addresses.json"

Here are the commands for internal consumption.  Much of the program logic goes 
into interactively constructing a value of type `Command` with the user.

> data Command
>   = New !String !String 
>   | Remove !String
>   | Search !String
>   | List
>   | Quit
>   deriving (Eq, Show)

This is the main program body.

> main =
>   setupState >>=
>   maybe
>     (putStrLn "addresses.json appears to be corrupt")
>     mainLoop
>
>   where

We use some mutual recursion to implement a control flow where the user can 
quit with the `Quit` command at any time.

>   mainLoop state = 
>     getInput >>= maybe (mainLoop state) (handleCommand state)
>
>   handleCommand state cmd = case cmd of
>     Quit -> return ()
>
>     List -> 
>       Map.traverseWithKey (curry $ putStrLn . toString) state >> 
>       mainLoop state 
>
>     Search str -> 
>       Map.traverseWithKey (testEntry str) state >>
>       mainLoop state
>
>     Remove name -> 
>       let state' =  Map.delete name state in
>       saveState state' >> mainLoop state'
>
>     New name addr -> 
>       let state' = Map.insert name addr state in
>       saveState state' >> mainLoop state'
>
>   testEntry str name addr
>      | str `isInfixOf` name 
>      = putStrLn . toString $ (name, addr)
>
>      | otherwise
>      = return ()
>
> toString (name, addr) = name ++ " @ " ++ addr

Try to grab the state from `addresses.json`, falling back to empty if it doesn't exist.

> setupState = 
>   doesFileExist dataFile >>=
>   bool (return . Just $ Map.empty) (Ae.decode <$> BSL.readFile dataFile) 

Save the state.

> saveState = BSL.writeFile dataFile . encode 


Here is the code for reading user input.

> getInput = do
>   putStrLn "\nWhat would you like to do?"
>   putStrLn "Commands: new, remove, search, list, quit"
>   getLine >>= parseCommand


> parseCommand input
>   | input == "new"
>   = do
>     putStrLn "Name: "
>     name <- getLine
>
>     putStrLn "Address: "
>     addr <- getLine
>
>     return . Just $ New name addr
>
>   | input == "remove"
>   = do
>     putStrLn "Name: "
>     name <- getLine
>
>     return . Just $ Remove name
>
>   | input == "search"
>   = do
>     putStrLn "Search phrase: "
>     phrase <- getLine
>
>     return . Just $ Search phrase
>
>   | input == "list"
>   = return . Just $ List 
>
>   | input == "quit"
>   = return . Just $ Quit 
>
>   | otherwise 
>   = return Nothing 
