-- | PURPOSE:
-- I use this to try stuff ChatGPT sugggested.

{-# LANGUAGE OverloadedStrings #-}

module GPT where

import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char


data Line = Properties
          | End
          | Title String
          | Heading Int String
          | Body String
  deriving (Show, Eq)

type Parser = Parsec Void String

propertiesParser :: Parser Line
propertiesParser =
  string ":PROPERTIES:" >> optional newline >> return Properties

endParser :: Parser Line
endParser =
  string ":END:" >> optional newline >> return End

titleParser :: Parser Line
titleParser = string "#+title:" >> space >>
              Title <$> manyTill anySingle newline

-- | PITFALL: Eats some whitespace.
headingParser :: Parser Line
headingParser = do
  numAsterisks <- some (char '*')
  space
  rest <- manyTill anySingle newline
  return ( Heading (length numAsterisks)
           $ T.unpack $ T.stripEnd $ T.pack rest )

bodyParser :: Parser Line
bodyParser = do
  line <- manyTill anySingle newline
  return $ Body (T.unpack $ T.stripEnd $ T.pack line)

lineParser :: Parser Line
lineParser = try propertiesParser
             <|> try endParser
             <|> try titleParser
             <|> try headingParser
             <|> bodyParser

parseLines :: String -> Either (ParseErrorBundle String Void) [Line]
parseLines input = parse (many lineParser) "" input
