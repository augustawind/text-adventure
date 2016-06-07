module Visualization 
    ( visualize
    , visualizeVerbose
    ) where

import Data.List (intercalate)
import qualified Data.Map as Map

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
      EndGame msg        -> formatOutput indentLevel output
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
