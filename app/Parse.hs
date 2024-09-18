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

ordinaryTextParser :: Parser OrdinaryText
ordinaryTextParser = OrdinaryText_text <$>
  some ( notFollowedBy -- TODO: Understand `notFollowedBy`.
         linkParser
         >> anySingleBut '\n')


-- * Each line in the file is one of these.

lineContentParser :: Parser [OrdinaryText]
lineContentParser = many ( try ordinaryTextParser
                           <|> linkParser )

propertiesStartParser :: Parser Line
propertiesStartParser = string ":PROPERTIES:"
                        >> return PropertiesStart

propertiesEndParser :: Parser Line
propertiesEndParser = string ":END:"
                      >> return PropertiesEnd

idParser :: Parser Line
idParser = string ":ID:" >> space >>
           Id <$> some (anySingleBut '\n')

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

parseFile :: FilePath -> IO ( Either
                               (ParseErrorBundle String Void)
                               [(LineNumber, Line)] )
parseFile filename = do
  input <- readFile filename
  return $ zip [1..] <$> parse (many lineParser) filename input
