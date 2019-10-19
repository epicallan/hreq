module Network.HTTP.Hreq
  ( module Network.Core.API
  , module Network.Core.Http
  , runHreq
  , runHreqDef
  , defaultConfig
  , HttpConfig (..)
  ) where

import Network.Core.API
import Network.Core.Http

import Network.HTTP.Hreq.Config (HttpConfig (..), defaultConfig)
import Network.HTTP.Hreq.Internal (runHreq, runHreqDef)
