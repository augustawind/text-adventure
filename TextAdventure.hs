module TextAdventure where

import Control.Monad (mapM_)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.IO


data Adventure = End
               | Do Action Adventure
               | Prompt String Switch

type Action = GameState -> IO ()
type Switch = Map.Map String Adventure

data GameState = GameState
    { getVars :: Vars
    , getPromptChars :: String
    , getTextWidth :: Int
    , getLineChar :: Char
    } deriving (Show, Read, Eq)

type Vars = Map.Map String String

-- Example usage.
-- ---------------------------------------------------------------------------

main = run mygame myAdventure

-- GameState:

mygame :: GameState
mygame = defaultGameState

-- Adventure:

myAdventure :: Adventure
myAdventure =
    Do intro $
        Prompt "Which direction will you take?" $
            Map.fromList [("left", Do goLeft End), ("right", Do goRight End)]

intro game = do
    printLines ["You've decided to set out on an adventure."
                ,"You've left your house and taken the path to a crossroads."]

    name <- prompt game "What is your name?"
    
    printWrap game ("Hello, " ++ name ++ "! Your adventure begins...")
    pause

    hr game

goLeft game = printWrap game "You went left!"
goRight game = printWrap game "You went right. A giant boar gores you."

-- Control flow.
-- ---------------------------------------------------------------------------

-- Run an Adventure.
run :: GameState -> Adventure -> IO ()
run game End = printWrap game "Game over!"
run game (Do action adventure) = action game >> run game adventure
run game this@(Prompt msg switch) = do
    let switch' = Map.mapKeys normalize switch

    choice <- cmdPrompt game (Map.keys switch') msg
    case Map.lookup choice switch' of
      Nothing        -> retry (run game this)
      Just adventure -> run game adventure

-- Same as prompt, but also takes a list of possible choices and
-- prints them, normalizing the input (see `normalize`).
cmdPrompt :: GameState -> [String] -> String -> IO String
cmdPrompt game choices msg = do putStr str
                                choice <- getLine
                                blankLine
                                return $ normalize choice

   where str = wordWrap width msg ++ ('\n':choicesStr) ++ (' ':promptChars)
         choicesStr = "(" ++ intercalate ", " choices ++ ")" 
         width = getTextWidth game
         promptChars = getPromptChars game

-- Print something then prompt for input. 
prompt :: GameState -> String -> IO String
prompt game message = do putStr str
                         answer <- fmap strip getLine
                         blankLine
                         return answer
    where str = wordWrap width message ++ ('\n':promptChars)
          width = getTextWidth game
          promptChars = getPromptChars game


-- Print a "try again" message and execute a given IO action.
retry :: IO () -> IO ()
retry action = putStrLn "Invalid input. Please try again." >> blankLine >> action

-- Pause execution and wait for a keypress to continue.
pause :: IO ()
pause = putStr "<Press any key to continue...>" >> getChar >> return ()

-- Output.
-- ---------------------------------------------------------------------------

-- Default options.
defaultGameState = GameState
    { getVars = Map.empty
    , getPromptChars = ">> "
    , getTextWidth = 78
    , getLineChar = '-'
    }

-- Print a blank line.
blankLine :: IO ()
blankLine = putChar '\n'

-- Print a horizontal rule.
hr :: GameState -> IO ()
hr game = putStrLn (replicate width char) >> blankLine
    where width = getTextWidth game
          char = getLineChar game

-- Print a list of Strings line by line.
printLines :: [String] -> IO ()
printLines xs = mapM_ putStrLn xs >> blankLine

-- Print a String, wrapping its text to the given width.
printWrap :: GameState -> String -> IO ()
printWrap game str = putStrLn (wordWrap (getTextWidth game) str) >> blankLine

-- String manipulation helpers.
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
