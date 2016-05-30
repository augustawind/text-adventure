module Visualization where

import Control.Monad (mapM_)
import Data.List (intercalate)
import qualified Data.Map as Map

import TextAdventure

visualize :: Adventure -> IO ()
visualize = mapM_ putStrLn . lines . showAdventure 0 

showAdventure :: Int -> Adventure -> String
showAdventure indentLevel (Node output disp)
    | Map.null disp = indentOutput indentLevel output
    | otherwise     = indentOutput indentLevel output ++
                        Map.foldlWithKey foldingFunction "" disp
    where
       foldingFunction result nodeName node =
           result ++
               "\n" ++ newIndentStr ++ showNodeName nodeName ++
               "\n" ++ showAdventure newIndentLevel node
       showNodeName name = "|--> " ++ name
       newIndentLevel = indentLevel + 4
       newIndentStr = replicate newIndentLevel ' '

indentOutput :: Int -> Output -> String
indentOutput indentLevel =
        intercalate "\n" . map (indentStr ++) . lines . wrapLines
    where
        wrapLines = intercalate "\n" . map wrapIndented . showOutput
        wrapIndented = wordWrap (78 - indentLevel)
        indentStr = replicate indentLevel ' '

showOutput :: Output -> [String]
showOutput o = case o of
                  Sequence xs   -> concatMap showOutput xs
                  _             -> [show o]
