{-# LANGUAGE OverloadedStrings #-}

module Parse where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Types


type Parser = Parsec Void String


-- * Parsers for within header or body text

linkParser :: Parser OrdinaryText
linkParser = do
  _    <- string "[[:id:"
  uri  <- many $ anySingleBut ']'
  _    <- string "]["
  name <- many $ anySingleBut ']'
  _    <- string "]]"
  return $ OrdinaryText_link uri name

textParser :: Parser OrdinaryText
textParser = do
  text <- manyTill anySingle $ lookAhead $
          choice [ newline           >> return ()
                 , lineContentParser >> return ()
                 , eof               >> return () ]
  return $ OrdinaryText_text text


-- * Each line in the file is one of these.

lineContentParser :: Parser [OrdinaryText]
lineContentParser = many (try textParser <|> linkParser)

propertiesStartParser :: Parser Line
propertiesStartParser = string ":PROPERTIES:"
                        >> return PropertiesStart

propertiesEndParser :: Parser Line
propertiesEndParser = string ":END:"
                      >> return PropertiesEnd

titleParser :: Parser Line
titleParser = string "#+title:" >>
              Title <$> many (anySingleBut '\n')

headingParser :: Parser Line
headingParser = do
  numAsterisks <- some (char '*')
  _ <- char ' '
  rest <- many (anySingleBut '\n')
  return $ Heading (length numAsterisks) rest

bodyParser :: Parser Line
bodyParser = Body <$> many (anySingleBut '\n')

lineParser :: Parser Line
lineParser = choice [ try propertiesStartParser
                    , try propertiesEndParser
                    , try titleParser
                    , try headingParser
                    , bodyParser ]

parseLines :: String ->
  Either (ParseErrorBundle String Void) [Line]
parseLines input = parse (many lineParser) "" input
