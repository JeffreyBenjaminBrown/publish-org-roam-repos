module Types where


-- * In use

type LineNumber = Int

type URI = String

data Link = Link URI String
  deriving (Show, Eq)

data NormalText = NormalText_text String
                | NormalText_link Link
  deriving (Show, Eq)

data Line = Line_PropsStart
          | Line_PropsEnd
          | Line_URI String
          | Line_Title String
          | Line_Headline Int [NormalText]
          | Line_Body         [NormalText]
  deriving (Show, Eq)

data Repo = Repo {
  repo_name :: String,
  repo_local_path :: FilePath, -- ^ absolute
  repo_online_path :: FilePath -- ^ absolute
  }
  deriving (Show, Eq)

data Node = Node {
  node_uri  :: URI,
  node_repo :: Repo,
  node_file :: FilePath, -- ^ relative
  node_line :: Maybe Int -- ^ Nothing for the URI of the whole file,
                         -- Just for URI of a headline within it.
  }
  deriving (Show, Eq)
