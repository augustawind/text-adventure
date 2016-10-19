module TextAdventure.Internal.TemplateString
    ( (-%-)
    , format
    ) where

import           Control.Monad                 (foldM)
import qualified Data.Map                      as Map

import           Text.ParserCombinators.Parsec (Parser, anyChar, between, char,
                                                eof, lookAhead, many, many1,
                                                manyTill, noneOf, parse, string,
                                                try, (<|>))

-- | @format@ as an infix operator.
infixl 4 -%-
(-%-) :: String -> Map.Map String String -> Either String String
str -%- vars = format str vars

-- | Parse a template string, replacing each variable (denoted by "%(var)")
-- with a corresponding value in the given Map. Returns 'Left err' if
-- a name doesn't exist in the Map.
format :: String -> Map.Map String String -> Either String String
format ""  _    = Right ""
format str vars = foldM f "" (parseTemplate str)
    where
        f acc (plainText, "") = Right $ acc ++ plainText
        f acc (plainText, var) =
            case Map.lookup var vars of
              Nothing -> Left $ "Name '" ++ var ++ "' doesn't exist"
              Just name -> Right $ acc ++ plainText ++ name

-- Run the @pairs@ parser on a string, returning the value within the
-- 'Right value. OK to throw an error on 'Left err' since the @pairs@ parser
-- always succeeds.
parseTemplate :: String -> [(String, String)]
parseTemplate str = either (error . show) id $ parse templatePairs source str
    where source = "<template string>: " ++ str

-- Parse a String into its @templatePair@s, returning list of 2-tuples.
-- This parser will never fail.
templatePairs :: Parser [(String, String)]
templatePairs = templatePair `manyTill` eof

-- Parse a @templateVar@, and return the text leading it up to it and the
-- @templateVar@ with its template formatting stripped, in a 2-tuple.
templatePair :: Parser (String, String)
templatePair = try ((,) <$> anythingBut templateVar <*> try templateVar) <|>
                   ((,) <$> many anyChar <*> string "")

-- Parse a template variable, in the format "%(myTemplateVar)". Returns
-- the text between the parentheses.
templateVar :: Parser String
templateVar = char '%' *> anyBetweenChars '(' ')'

-- Parse anything that is not matched by the given parser. Boolean @not@
-- for parsers.
anythingBut :: Parser String -> Parser String
anythingBut p = try (anyChar `manyTill` lookAhead p) <|> many anyChar

-- Match anything between Chars @open@ and @close@, but not nothing.
anyBetweenChars :: Char -> Char -> Parser String
anyBetweenChars open close =
    between (char open) (char close) $ many1 $ noneOf [close]
