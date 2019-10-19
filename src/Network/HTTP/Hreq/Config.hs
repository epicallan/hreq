module  Network.HTTP.Hreq.Config where

import Network.Core.Http

data HttpConfig = HttpConfig
  { base    :: BaseUrl
  , manager :: String
  }

defaultConfig :: HttpConfig
defaultConfig = undefined

--  default Manager
