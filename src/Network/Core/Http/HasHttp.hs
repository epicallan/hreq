{-# LANGUAGE AllowAmbiguousTypes #-}
module Network.Core.Http.HasHttp where

import Data.Proxy
import Network.Core.Http.HasRequest
import Network.Core.Http.HasResponse
import Network.Core.Http.Request
import Network.Core.Http.RunHttp

type HasHttp api m = (HasRequest api m, HasResponse api m, RunHttp m)

hreq'
  :: forall api m. HasHttp api m
  => Proxy api
  -> HttpInput api
  -> m (HttpOutput api)
hreq' api input = do
  res <- httpInput api input defaultRequest
  httpRes api res

hreq
  :: forall api m . HasHttp api m
  => HttpInput api
  -> m (HttpOutput api)
hreq = hreq' (Proxy @api)
