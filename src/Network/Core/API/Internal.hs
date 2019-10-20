module Network.Core.API.Internal
  ( module Network.Core.API.Request
  , module Network.Core.API.Response
  , Api (..)
  , Sing (SRes, SReq)
  , (:>)
  , (:?)
  , (:=)
  ) where


import Data.Kind
import Data.Singletons
import GHC.TypeLits

import Network.Core.API.Request
import Network.Core.API.Response

data Api a =
    Req [ ReqContent a]
  | Res [ ResContent a ]

data instance Sing (a :: Api k) where
  SReq :: Sing content -> Sing ('Req content)
  SRes :: Sing content -> Sing ('Res content)

instance (SingI content) => SingI ('Res content) where
  sing = SRes sing

instance (SingI content) => SingI ('Req content) where
  sing = SReq sing


-- | API Helper type combinators
infixr 6 ::>

data (a :: k1) ::> (b :: k2)

infixr 7 :>

type (xs :: [ReqContent Type]) :> (content :: k ) = xs ::> content

infixr 5 :?

type (a :: Symbol) :? (ts :: k) = a ::> ts

infixr 1 :=

type (a :: k1) := (b :: k2) = '( a, b)
