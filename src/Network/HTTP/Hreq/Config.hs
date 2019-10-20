module Network.HTTP.Hreq.Config where

import Network.Core.Http
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as TLS

data HttpConfig = HttpConfig
  { baseUrl :: BaseUrl
  , manager :: C.Manager
  }

createDefConfig :: BaseUrl -> IO HttpConfig
createDefConfig baseUrl@(BaseUrl scheme _ _ _) =
  HttpConfig baseUrl <$> manager'
  where
    manager' :: IO C.Manager
    manager' = case scheme of
       Http  -> C.newManager C.defaultManagerSettings
       Https -> C.newManager TLS.tlsManagerSettings
