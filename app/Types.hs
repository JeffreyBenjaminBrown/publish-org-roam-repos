module Types where


-- * In use

type URI = String

data LineContent = LineContent_text String
                 | LineContent_link URI String
  deriving (Show, Eq)

data Line = PropertiesStart
          | PropertiesEnd
          | Title String
          | Heading Int String
          | Body        String
  deriving (Show, Eq)


-- * Not in use

data Repo = Repo {
  name  ::  String,
  root  ::  FilePath }

data Node = Node {
  uri  :: URI,
  file :: FilePath,
  line :: Maybe Int -- ^ Nothing for the URI of the whole file,
                    -- Just for URI of a heading within it
  }
