module Network.Core.Http.BaseUrl where

data Scheme =
    Http
  | Https
  deriving (Show, Eq, Ord)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme   -- ^ URI scheme to use
  , baseUrlHost   :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort   :: Int      -- ^ port (eg 80)
  , baseUrlPath   :: String   -- ^ path (eg "/a/b/c")
  } deriving (Show, Eq, Ord)

showBaseUrl :: BaseUrl -> String
showBaseUrl = error "TODO: implement me"
