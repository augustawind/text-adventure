module TextAdventure where

import Control.Monad (mapM_, void)
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.IO

import TemplateString ((-%-)) 

-- GameState data.
-- ---------------------------------------------------------------------------

type GameAction a = StateT GameState IO a

data GameState = GameState
    { getVars :: Vars
    , getPromptChars :: String
    , getTextWidth :: Int
    , getLineChar :: Char
    } deriving (Show, Eq)

data Adventure = Node Output Branches
    deriving (Show, Eq)

type Branches = Map.Map Message Adventure

data Output = Print String 
            | PrintLines [String]
            | Prompt Var String
            | HR
            | BlankLine
            | Pause
            | Sequence [Output]
            deriving (Show, Eq)

type Vars = Map.Map Var String
type Var = String

-- | Semantic alias for Map.fromList.
branches :: [(String, Adventure)] -> Branches
branches = Map.fromList

-- | Semantic alias for Map.empty. Used to end the game on a particular @Node@.
gameOver :: [(String, Adventure)] -> Branches
gameOver = Map.empty

-- | Default game state.
defaultGameState :: GameState
defaultGameState = GameState
    { getVars = Map.empty
    , getPromptChars = ">> "
    , getTextWidth = 78
    , getLineChar = '-'
    }

-- Example usage.
-- ---------------------------------------------------------------------------

main :: IO ()
main = void $ runStateT (run myAdventure) myGameState

-- GameState:
myGameState :: GameState
myGameState = defaultGameState

-- Adventure:
myAdventure :: Adventure
myAdventure =
    Node $ Sequence [PrintLines intro,
                     Prompt "name", "What is your name?",
                     Print "Hello, %(name)! Your adventure begins...",
                     Pause, HR] $ branches $
       [("left",
            Node (Print "You went left. You found the treasure! You win!") gameOver)

       ,("right",
            Node (Print "You went right. A giant boar gores you.") gameOver)]

intro :: [String]
intro = ["You've decided to set out on an adventure."
        ,"You've left your house and taken the path to a crossroads."]

-- Control flow.
-- ---------------------------------------------------------------------------

-- | Run an Adventure.
run :: Adventure -> GameAction ()
run (Node output paths) = action >> next
    where
        action = case output of
                   Print str -> printWrap_ str
                   PrintLines strs -> printLines_ strs
                   Prompt var str -> runPrompt var str
                   HR -> hr_
                   BlankLine -> liftIO blankLine
                   Pause -> pause
                   Sequence outputs = sequence_ outputs
        next = if Map.empty paths
                  then return ()
                  else chooseBranch paths

runPrompt :: Var -> String -> GameAction ()
runPrompt var str = do
    answer <- prompt_ str
    if null answer
       then runPrompt var str
       else do
           game <- get
           let newVars = Map.insert var answer $ getVars game
           put $ game { getVars = newVars }

run this@(Node output switch) = do
    let switch' = Map.mapKeys normalize switch
    choice <- cmdPrompt_ (Map.keys switch') msg
    case Map.lookup choice switch' of
      Nothing        -> retry (run this)
      Just adventure -> run adventure

run (Pause adventure) = pause >> run adventure

run (Do action adventure) = action >> run adventure

-- | Same as @prompt@, but also takes a list of possible choices and
-- prints them, normalizing the input with @normalize@.
cmdPrompt :: [String] -> String -> GameAction String
cmdPrompt choices message = do
    game <- get
    printWrap message
    liftIO $ do
        let choicesStr = "(" ++ intercalate ", " choices ++ ")" 
        putStr $ choicesStr ++ (' ' : getPromptChars game)
        choice <- getLine
        return $ normalize choice

-- | @cmdPrompt@ with a blank line added to the end.
cmdPrompt_ :: [String] -> String -> GameAction String
cmdPrompt_ cs msg = cmdPrompt cs msg >>= \x -> liftIO blankLine >> return x

-- | Print something then prompt for input.
prompt :: String -> GameAction String
prompt message = do
    game <- get
    printWrap message
    liftIO $ do
        putStr $ getPromptChars game
        answer <- fmap strip getLine
        return answer

-- | @prompt@ with a blank line added to the end.
prompt_ :: String -> GameAction String
prompt_ msg = prompt msg >>= \x -> liftIO blankLine >> return x

-- | Print a "try again" message and re-execute the given @GameAction@.
retry :: GameAction () -> GameAction ()
retry action = do
    liftIO $ putStrLn "Invalid input. Please try again." >> blankLine
    action 

-- | Pause execution and wait for a keypress to continue.
pause :: GameAction ()
pause = liftIO $
    putStr "(Press <Enter> to continue...)" >> void getLine

-- Output.
-- ---------------------------------------------------------------------------

-- | Print a list of Strings line by line, wrapping each line to the given width.
printLines :: [String] -> GameAction ()
printLines xs = mapM_ printWrap xs

-- @printLines@ with a blank line added to the end.
printLines_ :: [String] -> GameAction ()
printLines_ xs = printLines xs >> liftIO blankLine

-- | Print a String, wrapping its text to the given width.
printWrap :: String -> GameAction ()
printWrap str = do
    game <- get
    let newStr = either (error . ("Error: "++)) id (str -%- getVars game)
    liftIO $ putStrLn $ wordWrap (getTextWidth game) newStr

-- | @printWrap@ with a blank line added to the end.
printWrap_ :: String -> GameAction ()
printWrap_ str = printWrap str >> liftIO blankLine

-- | Print a horizontal rule.
hr :: GameAction ()
hr = do
    game <- get
    let width = getTextWidth game
        char = getLineChar game
    liftIO $ putStrLn (replicate width char)

-- | @hr@ with a blank line added to the end.
hr_ :: GameAction ()
hr_ = hr >> liftIO blankLine

-- | Print a blank line.
blankLine :: IO ()
blankLine = putChar '\n'

-- String helpers.
-- ---------------------------------------------------------------------------

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
