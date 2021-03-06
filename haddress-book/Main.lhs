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

Our program state will be the current mapping from names to addresses.

> type State = Map String String

This is the main program body.

> main :: IO ()
> main =
>   setupState >>=
>   maybe
>     (putStrLn "addresses.json appears to be corrupt")
>     mainLoop
>
>   where

We use some mutual recursion to implement a control flow where the user can 
quit with the `Quit` command at any time.

>   mainLoop :: State -> IO ()
>   mainLoop state = 
>     getInput >>= maybe (mainLoop state) (handleCommand state)
>
>   handleCommand :: State -> Command -> IO ()
>   handleCommand state cmd = case cmd of
>     -- return :: a -> IO a 
>     -- like Promise.resolve
>     Quit -> return () -- no-op
>
>     List -> 
>       Map.traverseWithKey (curry $ putStrLn . toString) state >> 
>       mainLoop state 
>
>     Search str -> 
>       Map.traverseWithKey (visitEntry str) state >>
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
>   visitEntry :: String -> String -> String -> IO ()
>   visitEntry str name addr
>      | str `isInfixOf` name 
>      = putStrLn . toString $ (name, addr)
>
>      | otherwise
>      = return ()
>
> toString :: (String, String) -> String
> toString (name, addr) = name ++ " @ " ++ addr

Try to grab the state from `addresses.json`, falling back to empty if it doesn't exist.

> setupState :: IO (Maybe State)
> setupState = 
>   doesFileExist dataFile >>=
>   bool (return . Just $ Map.empty) (Ae.decode <$> BSL.readFile dataFile) 

Save the state.

> saveState :: State -> IO ()
> saveState = BSL.writeFile dataFile . encode 


Here is the code for reading user input.

> getInput :: IO (Maybe Command)
> getInput = do
>   putStrLn "\nWhat would you like to do?"
>   putStrLn "Commands: new, remove, search, list, quit"
>   getLine >>= parseCommand


> parseCommand :: String -> IO (Maybe Command)
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
