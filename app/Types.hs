module Types where

import Data.Map (Map)
import Data.Void
import Text.Megaparsec (ParseErrorBundle)


type LineNumber = Int

type Anchor = String {- ^ In a URL like https://github.com/user/repo/blob/main/file.org#abc, the part after the # is the "anchor". It might be absent. -}

type URI = String -- ^ Unique org-roam identifiers.

type MPError = ParseErrorBundle String Void

data Link = Link URI String
  deriving (Show, Eq)

data NormalText = NormalText_text String
                | NormalText_link Link
  deriving (Show, Eq)

-- ^ An org headline.
data Headline = Headline Int [NormalText]
  deriving (Show, Eq)

data Line = Line_PropsStart
          | Line_PropsEnd
          | Line_URI String
          | Line_Title String
          | Line_Headline Headline
          | Line_Body [NormalText]
  deriving (Show, Eq)

data Repo = Repo
  { repo_local_source       :: FilePath -- ^ absolute
  , repo_local_destination  :: FilePath -- ^ absolute
  , repo_online_destination :: FilePath -- ^ absolute
  } deriving (Show, Eq)

data Node = Node
  { node_repo   :: Repo
  , node_file   :: FilePath -- ^ relative
  , node_anchor :: Maybe Anchor
    -- ^ Nothing if the URI is for the whole file.
    -- Just if the URI is for a headline within it.
  } deriving (Show, Eq)

type Index = Map URI Node
