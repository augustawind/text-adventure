module TextAdventure where

import Control.Monad (mapM_)
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.IO

-- Game data.
-- ---------------------------------------------------------------------------

data Adventure = End
               | Do Action Adventure
               | Prompt String Switch

type Action = StateT Game IO ()
type Switch = Map.Map String Adventure

data Game = Game
    { getVars :: Vars
    , getPromptChars :: String
    , getTextWidth :: Int
    , getLineChar :: Char
    } deriving (Show, Read, Eq)

type Vars = Map.Map String String

-- Default options.
defaultGame = Game
    { getVars = Map.empty
    , getPromptChars = ">> "
    , getTextWidth = 78
    , getLineChar = '-'
    }

-- Example usage.
-- ---------------------------------------------------------------------------

main :: IO ()
main = runStateT (run myAdventure) myGame >> return ()

-- Game:

myGame :: Game
myGame = defaultGame

-- Adventure:

myAdventure :: Adventure
myAdventure =
    Do intro $
        Prompt "Which direction will you take?" $
            Map.fromList [("left", Do goLeft End), ("right", Do goRight End)]

intro :: StateT Game IO ()
intro = do
    printLines_ ["You've decided to set out on an adventure."
                ,"You've left your house and taken the path to a crossroads."]

    name <- prompt "What is your name?"
    
    printWrap_ ("Hello, " ++ name ++ "! Your adventure begins...")
    pause

    hr_

goLeft :: StateT Game IO ()
goLeft = printWrap_ "You went left. You found the treasure! You win!"

goRight :: StateT Game IO ()
goRight = printWrap_ "You went right. A giant boar gores you."

-- Control flow.
-- ---------------------------------------------------------------------------

-- Run an Adventure.
run :: Adventure -> StateT Game IO ()
run End = printWrap "Game over!"
run (Do action adventure) = action >> run adventure
run this@(Prompt msg switch) = do
    let switch' = Map.mapKeys normalize switch

    choice <- cmdPrompt_ (Map.keys switch') msg
    case Map.lookup choice switch' of
      Nothing        -> retry (run this)
      Just adventure -> run adventure

-- Same as prompt, but also takes a list of possible choices and
-- prints them, normalizing the input (see `normalize`).
cmdPrompt :: [String] -> String -> StateT Game IO String
cmdPrompt choices msg = do
    game <- get
    let str = wordWrap width msg ++ ('\n':choicesStr) ++ (' ':promptChars)
        choicesStr = "(" ++ intercalate ", " choices ++ ")" 
        width = getTextWidth game
        promptChars = getPromptChars game
    liftIO $ do
        putStr str
        choice <- getLine
        return $ normalize choice

-- @cmdPrompt@ with a blank line added to the end.
cmdPrompt_ :: [String] -> String -> StateT Game IO String
cmdPrompt_ cs msg = cmdPrompt cs msg >>= \x -> liftIO blankLine >> return x

-- Print something then prompt for input. 
prompt :: String -> StateT Game IO String
prompt message = do
    game <- get
    let str = wordWrap width message ++ ('\n':promptChars)
        width = getTextWidth game
        promptChars = getPromptChars game
    liftIO $ do putStr str
                answer <- fmap strip getLine
                return answer

-- @prompt@ with a blank line added to the end.
prompt_ :: String -> StateT Game IO String
prompt_ msg = prompt msg >>= \x -> liftIO blankLine >> return x


-- Print a "try again" message and execute a given IO action.
retry :: StateT Game IO () -> StateT Game IO ()
retry action = do
    liftIO $ putStrLn "Invalid input. Please try again." >> blankLine
    action 
    return ()

-- Pause execution and wait for a keypress to continue.
pause :: StateT Game IO ()
pause = liftIO $
    putStr "<Press any key to continue...>" >> getChar >> return ()

-- Output.
-- ---------------------------------------------------------------------------

-- Print a blank line.
blankLine :: IO ()
blankLine = putChar '\n'

-- Print a horizontal rule.
hr :: StateT Game IO ()
hr = do
    game <- get
    let width = getTextWidth game
        char = getLineChar game
    liftIO $ putStrLn (replicate width char)

-- @hr@ with a blank line added to the end.
hr_ :: Action
hr_ = hr >> liftIO blankLine

-- Print a list of Strings line by line, wrapping each line to the given width.
printLines :: [String] -> StateT Game IO ()
printLines xs = mapM_ printWrap xs

-- @printLines@ with a blank line added to the end.
printLines_ :: [String] -> StateT Game IO ()
printLines_ xs = printLines xs >> liftIO blankLine

-- Print a String, wrapping its text to the given width.
printWrap :: String -> StateT Game IO ()
printWrap str = do
    game <- get
    liftIO $ putStrLn (wordWrap (getTextWidth game) str)

-- @printWrap@ with a blank line added to the end.
printWrap_ :: String -> Action
printWrap_ str = printWrap str >> liftIO blankLine

-- String helpers.
-- ---------------------------------------------------------------------------

-- Remove leading and trailing whitespace from a String.
strip :: String -> String
strip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
    where isWhitespace = (`elem` [' ', '\t', '\n', '\r'])

-- Strip leading and trailing whitespace and make everything lowercase.
normalize :: String -> String
normalize = map toLower . strip

-- Wrap a String to fit the given width, adding line breaks as necessary.
-- This removes all pre-existing line breaks.
wordWrap :: Int -> String -> String
wordWrap width str
  | length str' <= width = str'
  | otherwise = take width str' ++ "\n" ++ wordWrap width (drop width str')
  where str' = filter notLineBreak str
        notLineBreak = not . (`elem` "\n\r")
