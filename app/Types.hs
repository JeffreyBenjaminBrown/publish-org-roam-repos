module Types where


-- * In use

type LineNumber = Int

type URI = String

data OrdinaryText = OrdinaryText_text String
                  | OrdinaryText_link URI String
  deriving (Show, Eq)

data Line = PropertiesStart
          | PropertiesEnd
          | Id String
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
