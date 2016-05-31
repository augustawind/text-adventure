module Visualization where

import Control.Monad (mapM_)
import Data.List (intercalate)
import qualified Data.Map as Map

import TextAdventure
import TextAdventure (Adventure(..), Output(..), Nexus(..))
import StringUtils (wordWrap)

--------------------------------------------------------------------------------
--  Concise visualization.

visualize :: Adventure -> IO ()
visualize = mapM_ putStrLn . lines . showAdventure 0 

showAdventure :: Int -> Adventure -> String
showAdventure indentLevel (Node output nexus) =
    case nexus of
      EndGame msg        -> formatMessage indentLevel msg
      Dispatch msg paths -> formatMessage indentLevel msg ++
                                Map.foldlWithKey foldingFunction "" paths
    where
       foldingFunction result nodeName node =
           result ++
               "\n" ++ newIndentStr ++ showNodeName nodeName ++
               "\n" ++ showAdventure newIndentLevel node
       showNodeName name = "|--> " ++ name
       newIndentLevel = indentLevel + 4
       newIndentStr = replicate newIndentLevel ' '

formatMessage :: Int -> String -> String
formatMessage indentLevel =
    intercalate "\n" . map (indentStr ++) . lines . wrapIndented
    where
        wrapIndented = wordWrap (78 - indentLevel)
        indentStr = replicate indentLevel ' '

--------------------------------------------------------------------------------
--  Verbose visualization.

visualizeVerbose :: Adventure -> IO ()
visualizeVerbose = mapM_ putStrLn . lines . showAdventureVerbose 0 

showAdventureVerbose :: Int -> Adventure -> String
showAdventureVerbose indentLevel (Node output nexus) =
    case nexus of
      EndGame msg -> formatOutput indentLevel output
      Dispatch msg paths -> formatOutput indentLevel output ++
                                Map.foldlWithKey foldingFunction "" paths
    where
       foldingFunction result nodeName node =
           result ++
               "\n\n" ++ newIndentStr ++ showNodeName nodeName ++
               "\n\n" ++ showAdventureVerbose newIndentLevel node
       showNodeName name = "|--> " ++ name
       newIndentLevel = indentLevel + 4
       newIndentStr = replicate newIndentLevel ' '

formatOutput :: Int -> [Output] -> String
formatOutput indentLevel =
        intercalate "\n" . map (indentStr ++) . lines . wrapLines
    where
        wrapLines = intercalate "\n" . map wrapIndented . map show
        wrapIndented = wordWrap (78 - indentLevel)
        indentStr = replicate indentLevel ' '

-- Custom game over.
gameOver :: Nexus
gameOver = EndGame "Game over!"

-- Adventure:
myAdventure :: Adventure
myAdventure =
    Node [PrintLines ["You've decided to set out on an adventure."
                     ,"You've left your house and taken the path to a crossroads."]
         ,Prompt "name" [] "What is your name?"
         ,Print "Hello, %(name)! Your adventure begins..."
         ,Pause
         ,HR]
    $ ask "Which direction will you go?" $
        [("left",
           Node [PrintLines ["You went left."
                            ,"You see a strange object."]]
           $ ask "Do you pick it up?" $
               [("yes",
                   Node [Print ("You pick it up and instantly " ++
                                "attain enlightenment. You win!")]
                   gameOver
                )
               ,("no",
                   Node [Print ("Your skepticism precedes you. " ++
                                "You turn to leave the object behind " ++
                                "and you see a treasure chest apparate out of thin air.")]
                   $ ask "Do you wish to open it?" $
                       [("yes",
                           Node [Print "You found the treasure! You win!"] gameOver)

                       ,("no",
                           Node [PrintLines ["You have denied too many opportunities!"
                                            ,"You are being punished for your ungratefulness."
                                            ,"You lose!"]]
                           gameOver
                        )]
                )]
         )
        ,("right",
           Node [Print "You went right."
                ,Prompt "lastWords" ["help", "fuck"] "Any last words?"
                ,PrintLines ["A giant boar gores you."
                            ,"Before you die, you say \"%(lastWords)\"."
                            ,"You lose!"]]
           gameOver
         )]
