{-# LANGUAGE OverloadedStrings #-}

module Parse where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Types


type Parser = Parsec Void String

-- * Parsers for within header or body text

linkParser :: Parser NormalText
linkParser = do
  _    <- string "[[:id:"
  uri  <- many $ anySingleBut ']'
  _    <- string "]["
  name <- many $ anySingleBut ']'
  _    <- string "]]"
  return $ NormalText_link uri name

ordinaryTextParser :: Parser NormalText
ordinaryTextParser = NormalText_text <$>
  some ( notFollowedBy -- TODO: Understand `notFollowedBy`.
         linkParser
         >> anySingleBut '\n')


-- * Each line in the file is one of these.

lineContentParser :: Parser [NormalText]
lineContentParser = many ( try ordinaryTextParser
                           <|> linkParser )

propertiesStartParser :: Parser Line
propertiesStartParser = string ":PROPERTIES:"
                        >> return Line_PropsStart

propertiesEndParser :: Parser Line
propertiesEndParser = string ":END:"
                      >> return Line_PropsEnd

idParser :: Parser Line
idParser = string ":ID:" >> space >>
           Line_Id <$> some (anySingleBut '\n')

titleParser :: Parser Line
titleParser = string "#+title:" >>
              Line_Title <$> many (anySingleBut '\n')

headingParser :: Parser Line
headingParser = do
  numAsterisks <- some (char '*')
  _ <- char ' '
  rest <- lineContentParser
  return $ Line_Heading (length numAsterisks) rest

bodyParser :: Parser Line
bodyParser = Line_Body <$> lineContentParser

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
  return $ zip [1..] <$> ( parse (sepBy lineParser newline)
                           filename input )
