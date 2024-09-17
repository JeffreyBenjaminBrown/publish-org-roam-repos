{-# LANGUAGE OverloadedStrings #-}

module Parse where

import qualified Data.Text as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Types


type Parser = Parsec Void String


linkParser :: Parser LineContent
linkParser = do
  _    <- string "[[:id:"
  uri  <- manyTill anySingle (char ']')
  _    <- char '['
  name <- manyTill anySingle $ string "]]"
  return $ LineContent_link uri name

textParser :: Parser LineContent
textParser = do
  text <- manyTill anySingle $ lookAhead $
          choice [ newline           >> return ()
                 , lineContentParser >> return ()
                 , eof               >> return () ]
  return $ LineContent_text text

lineContentParser :: Parser [LineContent]
lineContentParser =
  manyTill (try linkParser <|> textParser)
  $ choice [ optional newline >> return ()
           , eof              >> return () ]


-- * Each line in the file is one of these.

propertiesStartParser :: Parser Line
propertiesStartParser = string ":PROPERTIES:" >> optional newline
                        >> return PropertiesStart

propertiesEndParser :: Parser Line
propertiesEndParser = string ":END:" >> optional newline
                      >> return PropertiesEnd

titleParser :: Parser Line
titleParser = string "#+title:" >> space >>
              Title <$> manyTill anySingle newline

headingParser :: Parser Line
headingParser = do
  numAsterisks <- some (char '*')
  char ' '
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
