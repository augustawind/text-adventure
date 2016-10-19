module TextAdventure.Internal.StringUtils
    ( wordWrap
    , normalize
    , strip
    , lstrip
    , rstrip
    ) where

import Data.Char (toLower)
import Data.List (dropWhileEnd)

-- | Wrap a String to fit the given width, adding line breaks as necessary.
-- This removes all pre-existing line breaks.
wordWrap :: Int -> String -> String
wordWrap width str
  | length str' <= width = str'
  | otherwise = rstrip firstLine ++ case wordWrap width rest of
                                      ""    -> ""
                                      rest' -> '\n':rest'
  where str' = filter notLineBreak . lstrip $ str
        notLineBreak = not . (`elem` "\n\r")
        (firstLine, rest) = splitAtWith isWhitespace width str'

-- | Split a list at the given width, or at the closest element that
-- fulfills the given predicate, whichever comes first. Return each
-- half of the split in a 2-tuple.
splitAtWith :: (a -> Bool) -> Int -> [a] -> ([a], [a])
splitAtWith p width xs = (first, rest)
    where
        first = case dropWhileEnd (not . p) chunk of
                  []   -> chunk
                  ys   -> init ys
        rest = drop (length first) $ xs
        chunk = take width xs

-- | Strip leading and trailing whitespace and make everything lowercase.
normalize :: String -> String
normalize = map toLower . strip

-- | Remove leading and trailing whitespace from a String.
strip :: String -> String
strip = lstrip . rstrip

-- | Remove leading whitespace from a String.
lstrip :: String -> String
lstrip = dropWhile isWhitespace

-- | Remove trailing whitespace from a String
rstrip :: String -> String
rstrip = dropWhileEnd isWhitespace

isWhitespace :: Char -> Bool
isWhitespace = (`elem` [' ', '\t', '\n', '\r'])
