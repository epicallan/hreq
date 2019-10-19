module Network.Core.API
    ( module Network.Core.API.Internal
    , module Network.Core.API.MediaType
    , module Network.Core.API.TypeLevel
    , module Network.Core.API.Verbs
    , module Network.Core.API
    ) where

import Data.Kind
import GHC.TypeLits

import Network.Core.API.Internal
import Network.Core.API.MediaType
import Network.Core.API.TypeLevel
import Network.Core.API.Verbs


-- | Type combinators

infixr 6 ::>

data (a :: k1) ::> (b :: k2)

infixr 7 :>

type (xs :: [ReqContent Type]) :> (content :: k ) = xs ::> content

infixr 5 :?

type (a :: Symbol) :? (ts :: k) = a ::> ts

infixr 1 :=

type (a :: k1) := (b :: k2) = '( a, b)

-- | Type synonyms

type QueryFlag (s :: Symbol) = '[ 'QueryFlags '[ s ] ]

type QueryFlags = 'QueryFlags

-----------------

type Param s t = '[ 'Params '[ s := t ] ]

type Params = 'Params

-------------------

type ReqBody = 'ReqBody

type JSONBody a  =  '[ ReqBody JSON a ]

---------------------

type ResBody = 'ResBody

type GetJSON a = Get '[ ResBody JSON a]

type PostJson a = Post '[ResBody JSON a]

----

-- | TODO: into DocTests
data User' = User'

type Example1 = JSONBody User' :> GetJSON User'

type Example6 = Param  "name" String :> GetJSON User'

type Example2 = "users" :? JSONBody User' :> GetJSON User'

type Example3 = "users"
  :? Param "author" String
  :> GetJSON User'

-- ^ returns only a single user output
type Example5 =
     Param "teacher" String
  :> Get '[ ResBody JSON User', 'ResStatus, 'ResHeaders '[ "content" := String] ]
   -- returns an HList with user, status code and header

type Example4 = "users"
  :? '[ Params '["some" := Int, "name" := String], ReqBody JSON User' ]
  :> GetJSON User'
