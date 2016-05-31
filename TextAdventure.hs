module TextAdventure
    -- | Data types.
    ( GameAction
    , GameState(..)
    , Adventure(..)
    , Dispatcher
    , Output(..)
    , Vars
    -- | Type constructors.
    , dispatcher
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
    -- | Output.
    , printLines, printLines_
    , printWrap, printWrap_
    , hr, hr_, blankLine
    -- | String utilities.
    , wordWrap
    , normalize
    , strip
    ) where

import Control.Monad (mapM, mapM_, sequence_, void)
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as Map
import System.IO

import TemplateString (format)
import StringUtils (wordWrap, normalize, strip)

--------------------------------------------------------------------------------
--  Game data.

-- | A GameAction performs IO with some associated @GameState@.
type GameAction a = StateT GameState IO a

-- | A record of the current game state.
data GameState = GameState
    -- | A @Data.Map.Map@ of game variables. Used by the user to
    -- store game state.
    { getVars :: Vars
    -- | The current prompt characters, such as ">> ".
    , getPromptChars :: String
    -- | The current text width to wrap to. This is unlikely to change.
    , getTextWidth :: Int
    -- | The current line character. Used for drawing a horizontal rule with @HR@.
    , getLineChar :: Char
    } deriving (Show, Eq)

-- | An adventure game.
-- This is a recursive tree structure where each Node has an arbitrary amount
-- of branches corresponding with each path the game can take at that point.
-- The Node itself represents a series of @Output@s as a list.
data Adventure = Node [Output] Dispatcher
    deriving (Show, Read, Eq)

-- | Type alias for a Map of choices to @Adventure@s. See @Adventure@.
type Dispatcher = Map.Map String Adventure

-- | Represents game output, such as printing a string, prompting for an answer,
-- or pausing the game.
data Output = Print String 
            | PrintLines [String]
            | Prompt Var [String] String
            | HR
            | BlankLine
            | Pause
            deriving (Show, Read, Eq)

-- | Semantic alias representing the game variables. See @GameState@.
type Vars = Map.Map Var String
-- | Semantic alias for a variable name.
type Var = String

-- | Semantic alias for Map.fromList.
dispatcher :: [(String, Adventure)] -> Dispatcher
dispatcher = Map.fromList

-- | Semantic alias for Map.empty. Used to end the game on a particular @Node@.
gameOver :: Dispatcher
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
play (Node outputs paths) = mapM_ toAction outputs >> next
    where
        next = if Map.null paths
                  then printGameOver 
                  else dispatch paths

-- | Given a @Dispatcher@ Map, prompt for an answer and then @play@ the
-- corresponding @Adventure@. Retry on invalid input.
dispatch :: Dispatcher -> GameAction ()
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
                    Prompt var choices str -> runPrompt var choices str
                    HR -> hr_
                    BlankLine -> liftIO blankLine
                    Pause -> pause

-- | Given a variable name and a message, print the message and then @prompt@
-- for an answer, updating the @getVars@ attribute of the current @GameState@
-- with the result. Retry when no input is given.
runPrompt :: Var -> [String] -> String -> GameAction ()
runPrompt var choices str = do
    answer <- if null choices
                then prompt_ str
                else printWrap str >> cmdPrompt_ choices

    if null answer || (not (null choices) && not (answer `elem` choices))
       then retry $ runPrompt var choices str
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
