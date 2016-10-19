{-# LANGUAGE OverloadedStrings #-}
module TextAdventure.Internal.TextUtils
    ( wordWrap
    , normalize
    ) where

import           Data.Maybe (isJust)
import           Data.Text  (Text, append, cons)
import qualified Data.Text  as Text

-- | Wrap a Text to fit the given width, adding line breaks as necessary.
-- This removes all pre-existing line breaks.
wordWrap :: Int -> Text -> Text
wordWrap width text
  | Text.length text' <= width = text'
  | otherwise = firstLine `append` case wordWrap width rest of
                                              ""    -> ""
                                              rest' -> '\n' `cons` rest'
  where text' = Text.filter notLineBreak text
        notLineBreak = (||) <$> (== '\n') <*> (== '\r')
        (firstLine, rest) = splitAtWith isWhitespace width text'

-- | Split Text at the given width, or at the closest char that
-- fulfills the given predicate, whichever comes first. Return each
-- half of the split in a 2-tuple.
splitAtWith :: (Char -> Bool) -> Int -> Text -> (Text, Text)
splitAtWith p width xs = (first, rest)
    where
        first = case Text.dropWhileEnd (not . p) chunk of
                  ""   -> chunk
                  ys   -> Text.init ys
        rest = Text.drop (Text.length first) xs
        chunk = Text.take width xs

-- | Strip leading and trailing whitespace and make everything lowercase.
normalize :: Text -> Text
normalize = Text.toLower . Text.strip

isWhitespace :: Char -> Bool
isWhitespace = (`elem` [' ', '\t', '\n', '\r'])
