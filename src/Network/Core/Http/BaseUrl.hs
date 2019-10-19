module Network.Core.Http.BaseUrl where

data Scheme =
    Http
  | Https
  deriving (Show, Eq, Ord)

data BaseUrl = BaseUrl
 { baseUrlScheme :: Scheme
 , baseUrlPath   :: String
 , baseUrlPort   :: Int
 } deriving (Show, Eq, Ord)

showBaseUrl :: BaseUrl -> String
showBaseUrl = undefined
