module TextAdventure
    -- | Data types.
    ( GameAction
    , GameState(..)
    , Adventure(..)
    , Branches
    , Output(..)
    , Vars
    -- | Type constructors.
    , branches
    , gameOver
    , defaultGameState
    -- | Game runners.
    , run
    , play
    -- | Control flow.
    , dispatch
    , toAction
    , runPrompt
    , cmdPrompt, cmdPrompt_
    , prompt, prompt_
    , pause, pause_
    , retry
    -- | Output
    , printLines, printLines_
    , printWrap, printWrap_
    , hr, hr_, blankLine
    -- | String manipulation.
    , wordWrap
    , normalize
    , strip
    ) where

import Control.Monad (mapM, mapM_, sequence_, void)
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.IO

import StringFormat (format)

--------------------------------------------------------------------------------
--  Game data.

type GameAction a = StateT GameState IO a

data GameState = GameState
    { getVars :: Vars
    , getPromptChars :: String
    , getTextWidth :: Int
    , getLineChar :: Char
    } deriving (Show, Eq)

data Adventure = Node Output Branches
    deriving (Show, Eq)

type Branches = Map.Map String Adventure

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
gameOver :: Branches
gameOver = Map.empty

-- | Default game state.
defaultGameState :: GameState
defaultGameState = GameState
    { getVars = Map.empty
    , getPromptChars = ">> "
    , getTextWidth = 78
    , getLineChar = '-'
    }

--------------------------------------------------------------------------------
--  Control flow.

-- | Run a game, given an @Adventure@ and a @GameState@.
run :: Adventure -> GameState -> IO ()
run adventure gameState = void $ runStateT (play adventure) gameState

-- | Convert an @Adventure@ to a @GameAction@.
play :: Adventure -> GameAction ()
play (Node output paths) = toAction output >> next
    where
        next = if Map.null paths
                  then printGameOver 
                  else dispatch paths

-- | Given a @Branches@ Map, prompt for an answer and then @play@ the
-- corresponding @Adventure@ branch. Retry on invalid input.
dispatch :: Branches -> GameAction ()
dispatch paths = do
    let paths' = Map.mapKeys normalize paths
    choice <- cmdPrompt_ (Map.keys paths')
    case Map.lookup choice paths' of
      Nothing        -> retry (dispatch paths)
      Just adventure -> play adventure

-- Print a game over message.
printGameOver :: GameAction ()
printGameOver = printWrap_ "Game over!"

-- | Convert an @Output@ to a @GameAction@.
toAction :: Output -> GameAction ()
toAction output = case output of
                    Print str -> printWrap_ str
                    PrintLines strs -> printLines_ strs
                    Prompt var str -> runPrompt var str
                    HR -> hr_
                    BlankLine -> liftIO blankLine
                    Pause -> pause
                    Sequence outputs -> mapM_ toAction outputs

-- | Given a variable name and a message, print the message and then @prompt@
-- for an answer, updating the @getVars@ attribute of the current @GameState@
-- with the result. Retry when no input is given.
runPrompt :: Var -> String -> GameAction ()
runPrompt var str = do
    answer <- prompt_ str
    if null answer
       then retry $ runPrompt var str
       else do
           game <- get
           let newVars = Map.insert var answer $ getVars game
           put $ game { getVars = newVars }

-- | Like @prompt@, but prints a given list of possible choices and prints
-- them, then prompts for a result, normalizing the input with @normalize@.
cmdPrompt :: [String] -> GameAction String
cmdPrompt choices = do
    game <- get
    liftIO $ do
        let choicesStr = "(" ++ intercalate ", " choices ++ ")" 
        putStr $ choicesStr ++ (' ' : getPromptChars game)
        choice <- getLine
        return $ normalize choice

-- | @cmdPrompt@ with a blank line added to the end.
cmdPrompt_ :: [String] -> GameAction String
cmdPrompt_ cs = cmdPrompt cs >>= \x -> liftIO blankLine >> return x

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

-- | @pause@ with a blank line added to the end.
pause_ :: GameAction ()
pause_ = pause >> liftIO blankLine

--------------------------------------------------------------------------------
--  Output.

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
    let newStr = either doError id formatted
        formatted = format str (getVars game)
        doError = error . ("Error: "++)
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

--------------------------------------------------------------------------------
--  String manipulation.

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
