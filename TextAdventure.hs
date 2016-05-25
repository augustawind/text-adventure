module TextAdventure where

import Control.Monad (mapM_, void)
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.IO

import TemplateString ((-%-)) 

-- Game data.
-- ---------------------------------------------------------------------------

data Adventure = End
               | Print Output Adventure
               | Prompt String String Adventure
               | CmdPrompt String (Map.Map String Adventure)
               | Pause Adventure
               | Do (Action ()) Adventure

data Output = Text String
            | Lines [String]
            | HR
            | BlankLine
            deriving (Eq, Show)

type Action a = StateT Game IO a

data Game = Game
    { getVars :: Vars
    , getPromptChars :: String
    , getTextWidth :: Int
    , getLineChar :: Char
    } deriving (Show, Read, Eq)

type Vars = Map.Map String String

-- | Default game state.
defaultGame :: Game
defaultGame = Game
    { getVars = Map.empty
    , getPromptChars = ">> "
    , getTextWidth = 78
    , getLineChar = '-'
    }

-- Example usage.
-- ---------------------------------------------------------------------------

main :: IO ()
main = void $ runStateT (run myAdventure) myGame

-- Game:

myGame :: Game
myGame = defaultGame

-- Adventure:
myAdventure :: Adventure
myAdventure =
    Print intro $
        Prompt "name" "What is your name?" $
            Print (Text "Hello, %(name)! Your adventure begins...") $
                Pause $ Print HR $
                    CmdPrompt "Which direction will you take?" $
                        Map.fromList [("left", Do goLeft End)
                                     ,("right", Do goRight End)]

intro :: Output
intro = Lines ["You've decided to set out on an adventure."
              ,"You've left your house and taken the path to a crossroads."]

goLeft :: Action ()
goLeft = printWrap_ "You went left. You found the treasure! You win!"

goRight :: Action ()
goRight = printWrap_ "You went right. A giant boar gores you."

-- Control flow.
-- ---------------------------------------------------------------------------

-- | Run an Adventure.
run :: Adventure -> Action ()

run End = printWrap "Game over!"

run (Print output adventure) = action >> run adventure
    where action = case output of
                     Text msg  -> printWrap_ msg
                     Lines xs  -> printLines_ xs
                     HR        -> hr_
                     BlankLine -> liftIO blankLine

run this@(Prompt var msg adventure) = do
    answer <- prompt_ msg
    if null answer
       then retry (run this)
       else do
           game <- get
           let newVars = Map.insert var answer $ getVars game
           put $ game { getVars = newVars }
           run adventure

run this@(CmdPrompt msg switch) = do
    let switch' = Map.mapKeys normalize switch
    choice <- cmdPrompt_ (Map.keys switch') msg
    case Map.lookup choice switch' of
      Nothing        -> retry (run this)
      Just adventure -> run adventure

run (Pause adventure) = pause >> run adventure

run (Do action adventure) = action >> run adventure

-- | Same as @prompt@, but also takes a list of possible choices and
-- prints them, normalizing the input with @normalize@.
cmdPrompt :: [String] -> String -> Action String
cmdPrompt choices message = do
    game <- get
    printWrap message
    liftIO $ do
        let choicesStr = "(" ++ intercalate ", " choices ++ ")" 
        putStr $ choicesStr ++ (' ' : getPromptChars game)
        choice <- getLine
        return $ normalize choice

-- | @cmdPrompt@ with a blank line added to the end.
cmdPrompt_ :: [String] -> String -> Action String
cmdPrompt_ cs msg = cmdPrompt cs msg >>= \x -> liftIO blankLine >> return x

-- | Print something then prompt for input.
prompt :: String -> Action String
prompt message = do
    game <- get
    printWrap message
    liftIO $ do
        putStr $ getPromptChars game
        answer <- fmap strip getLine
        return answer

-- | @prompt@ with a blank line added to the end.
prompt_ :: String -> Action String
prompt_ msg = prompt msg >>= \x -> liftIO blankLine >> return x

-- | Print a "try again" message and re-execute the given @Action@.
retry :: Action () -> Action ()
retry action = do
    liftIO $ putStrLn "Invalid input. Please try again." >> blankLine
    action 

-- | Pause execution and wait for a keypress to continue.
pause :: Action ()
pause = liftIO $
    putStr "(Press <Enter> to continue...)" >> void getLine

-- Output.
-- ---------------------------------------------------------------------------

-- | Print a list of Strings line by line, wrapping each line to the given width.
printLines :: [String] -> Action ()
printLines xs = mapM_ printWrap xs

-- @printLines@ with a blank line added to the end.
printLines_ :: [String] -> Action ()
printLines_ xs = printLines xs >> liftIO blankLine

-- | Print a String, wrapping its text to the given width.
printWrap :: String -> Action ()
printWrap str = do
    game <- get
    let newStr = either (error . ("Error: "++)) id (str -%- getVars game)
    liftIO $ putStrLn $ wordWrap (getTextWidth game) newStr

-- | @printWrap@ with a blank line added to the end.
printWrap_ :: String -> Action ()
printWrap_ str = printWrap str >> liftIO blankLine

-- | Print a horizontal rule.
hr :: Action ()
hr = do
    game <- get
    let width = getTextWidth game
        char = getLineChar game
    liftIO $ putStrLn (replicate width char)

-- | @hr@ with a blank line added to the end.
hr_ :: Action ()
hr_ = hr >> liftIO blankLine

-- | Print a blank line.
blankLine :: IO ()
blankLine = putChar '\n'

-- String helpers.
-- ---------------------------------------------------------------------------

fromTemplate :: String -> Map.Map String String -> String
fromTemplate "" _ = ""
fromTemplate str vars
  | Map.null vars = str
  | otherwise = undefined

-- Wrap a String to fit the given width, adding line breaks as necessary.
-- This removes all pre-existing line breaks.
wordWrap :: Int -> String -> String
wordWrap width str
  | length str' <= width = str'
  | otherwise = take width str' ++ "\n" ++ wordWrap width (drop width str')
  where str' = filter notLineBreak str
        notLineBreak = not . (`elem` "\n\r")

-- Strip leading and trailing whitespace and make everything lowercase.
normalize :: String -> String
normalize = map toLower . strip

-- Remove leading and trailing whitespace from a String.
strip :: String -> String
strip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
    where isWhitespace = (`elem` [' ', '\t', '\n', '\r'])
