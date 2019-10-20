module Network.Core.Http.Request where

import Data.ByteString as B
import Data.Monoid ((<>))
import GHC.Generics
import Network.HTTP.Types (Header, HeaderName, HttpVersion (..), Method, QueryItem, http11,
                           methodGet)
import Web.HttpApiData (ToHttpApiData (..))

data Request = Request
  { reqPath        :: String -- | TODO: Maybe use builder or Text
  , reqMethod      :: Method
  , reqBody        :: Maybe B.ByteString
  , reqQueryString :: [QueryItem]
  , reqHttpVersion :: HttpVersion
  , reqHeaders     :: [Header]
  } deriving (Eq, Show, Generic)

defaultRequest :: Request
defaultRequest = Request
  { reqPath = ""
  , reqMethod = methodGet
  , reqBody = Nothing
  , reqQueryString = []
  , reqHttpVersion = http11
  , reqHeaders = []
  }

appendMethod :: Method -> Request -> Request
appendMethod method req = req { reqMethod = method }

appendToPath :: String -> Request -> Request
appendToPath p req = req { reqPath = reqPath req <> p }

appendToQueryString
  :: QueryItem
  -> Request
  -> Request
appendToQueryString queryItem req =
  req { reqQueryString = queryItem : reqQueryString req }
  -- ^ TODO: we probably need to reverse the query list

addHeader :: ToHttpApiData a => HeaderName -> a -> Request -> Request
addHeader name val req =
  req {reqHeaders = (name, toHeader val) : reqHeaders req}

setReqBody :: ByteString -> Request -> Request
setReqBody b req = req { reqBody = Just b }
