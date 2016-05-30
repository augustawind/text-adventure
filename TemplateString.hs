module TemplateString
    ( (-%-)
    , format
    ) where

import Control.Monad (foldM)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec (Parser, parse, try, lookAhead, eof,
                                      anyChar, char, string, many, many1,
                                      noneOf, manyTill, between, (<|>))

-- | @format@ as an infix operator.
infixl 4 -%-
(-%-) :: String -> Map.Map String String -> Either String String
str -%- vars = format str vars

-- | Parse a template string, replacing each variable (denoted by "%(var)")
-- with a corresponding value in the given Map. Returns 'Left err' if
-- a name doesn't exist in the Map.
format :: String -> Map.Map String String -> Either String String
format ""  _    = Right ""
format str vars = foldM f "" (extractPairs str) 
    where
        f acc (plainText, "") = Right $ acc ++ plainText
        f acc (plainText, var) =
            case Map.lookup var vars of
              Nothing -> Left $ "Name '" ++ var ++ "' doesn't exist"
              Just name -> Right $ acc ++ plainText ++ name

-- Run the @pairs@ parser on a string, returning the value within the
-- 'Right value. OK to throw an error on 'Left err' since the @pairs@ parser
-- always succeeds.
extractPairs :: String -> [(String, String)]
extractPairs str = either (error . show) id $ parse pairs source str
    where source = "<template string>: " ++ str

-- Parse each @pair@ of (plain text, template var) into a list of 2-tuples.
pairs :: Parser [(String, String)]
pairs = pair `manyTill` eof

-- Parse a @templateVar@, and return the text leading it up to it and the
-- @templateVar@ with its template formatting stripped, in a 2-tuple.
pair :: Parser (String, String)
pair = try ((,) <$> anythingBut templateVar <*> try templateVar) <|>
           ((,) <$> many anyChar <*> string "")

-- Parse a template variable, in the format "%(myTemplateVar)". Returns
-- the text between the parentheses.
templateVar :: Parser String
templateVar = char '%' *> anyBetweenChars '(' ')'

-- Parse anything that is not matched by the given parser. Boolean @not@
-- for parsers.
anythingBut :: Parser String -> Parser String
anythingBut p = anyChar `manyTill` (lookAhead (try p)) <|> many anyChar

-- Match anything between Chars @open@ and @close@, but not nothing.
anyBetweenChars :: Char -> Char -> Parser String
anyBetweenChars open close =
    between (char open) (char close) $ many1 $ noneOf [close]
