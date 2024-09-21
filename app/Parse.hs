{-# LANGUAGE OverloadedStrings #-}

module Parse where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import Types


parseFile :: FilePath -> IO ( Either MPError [Line] )
parseFile filename = do
  input <- readFile filename
  return ( parse
           (sepBy lineParser newline)
           filename input )


-- * INTERNAL
-- The rest of this is used only above and in tests.

type Parser = Parsec Void String

-- * Parsers for within header or body text

linkParser :: Parser NormalText
linkParser = do
  _    <- string "[[id:"
  uri  <- many $ anySingleBut ']'
  _    <- string "]["
  name <- many $ anySingleBut ']'
  _    <- string "]]"
  return $ NormalText_link $ Link uri name

-- | todo: understand @notFollowedBy@, used herein.
ordinaryTextParser :: Parser NormalText
ordinaryTextParser = NormalText_text <$>
  some ( notFollowedBy
         linkParser
         >> anySingleBut '\n')


-- * Each line in the file is one of these.

lineContentParser :: Parser [NormalText]
lineContentParser = many ( try ordinaryTextParser
                           <|> linkParser )

propertiesStartParser :: Parser Line
propertiesStartParser = optional space >> string ":PROPERTIES:"
                        >> return Line_PropsStart

propertiesEndParser :: Parser Line
propertiesEndParser = optional space >> string ":END:"
                      >> return Line_PropsEnd

uriParser :: Parser Line
uriParser = optional space >> string ":ID:" >> space >>
           Line_URI <$> some (anySingleBut '\n')

titleParser :: Parser Line
titleParser = string "#+title:" >>
              Line_Title <$> many (anySingleBut '\n')

headlineParser :: Parser Line
headlineParser = do
  numAsterisks <- some (char '*')
  _ <- char ' '
  rest <- lineContentParser
  return $ Line_Headline $ Headline (length numAsterisks) rest

bodyParser :: Parser Line
bodyParser = Line_Body <$> lineContentParser

lineParser :: Parser Line
lineParser = choice [ try propertiesStartParser
                    , try propertiesEndParser
                    , try uriParser
                    , try titleParser
                    , try headlineParser
                    , bodyParser ]
