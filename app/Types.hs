module Types where


-- * In use

type LineNumber = Int

type URI = String

data NormalText = NormalText_text String
                  | NormalText_link URI String
  deriving (Show, Eq)

data Line = Line_PropsStart
          | Line_PropsEnd
          | Line_Id String
          | Line_Title String
          | Line_Heading Int [NormalText]
          | Line_Body        [NormalText]
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
