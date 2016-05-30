module StringUtils
    ( wordWrap
    , normalize
    , strip
    ) where

import Data.Char (toLower)

-- | Wrap a String to fit the given width, adding line breaks as necessary.
-- This removes all pre-existing line breaks.
wordWrap :: Int -> String -> String
wordWrap width str
  | length str' <= width = str'
  | otherwise = firstLine ++ "\n" ++ wordWrap width rest
  where str' = filter notLineBreak str
        notLineBreak = not . (`elem` "\n\r")
        (firstLine, rest) = smartLineBreak width str'

-- Split a String at the given width (at most), or at the closest word
-- before that width. Words that are longer than the given width will be split.
smartLineBreak :: Int -> String -> (String, String)
smartLineBreak width str = (first, rest)
    where 
        first =
            case (reverse . dropWhile (not . isWhitespace) . reverse) chunk of
                ""   -> chunk
                str' -> str'
        rest = dropWhile isWhitespace . drop (length first) $ str
        chunk = take width str

-- | Strip leading and trailing whitespace and make everything lowercase.
normalize :: String -> String
normalize = map toLower . strip

-- | Remove leading and trailing whitespace from a String.
strip :: String -> String
strip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace

isWhitespace :: Char -> Bool
isWhitespace = (`elem` [' ', '\t', '\n', '\r'])
