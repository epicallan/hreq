module Network.Core.API
    ( module Network.Core.API.Internal
    , module Network.Core.API.MediaType
    , module Network.Core.API.TypeLevel
    , module Network.Core.API.Verbs
    , module Network.Core.API
    , Header
    , Status (..)
    ) where

import GHC.TypeLits

import Network.Core.API.Internal
import Network.Core.API.MediaType
import Network.Core.API.TypeLevel
import Network.Core.API.Verbs
import Network.HTTP.Types (Header, Status (..))

-- | Single Request item Type synonyms

type QueryFlag (s :: Symbol) = '[ QueryFlags '[ s ] ]

type Param s t = '[ Params '[ s := t ] ]

type Capture s t = '[ Captures '[ s := t] ]

type CaptureAll' s t = '[ CaptureAll s t ]

type JSONBody a  =  '[ ReqBody JSON a ]

type ResHeader s t = '[ ResHeaders '[ s := t] ]

-- | Response Type synonyms

type GetJSON a = Get '[ ResBody JSON a]

type PostJSON a = Post '[ResBody JSON a]

type PutJSON a = Put '[ ResBody JSON a]

type PatchJSON a = Patch '[ ResBody JSON a]

type DeleteJson a = Delete '[ResBody JSON a]

type RawResponse v = Verb v '[ Raw ]

type EmptyResponse v = Verb v '[]

-- | Doc-tests
--
-- >>> data User = User

-- >>> type Ex1 = JSONBody User :> GetJSON User

-- >>> type Ex2 = Param  "name" String :> GetJSON User

-- >>> type Ex3 = "users" :? JSONBody User :> GetJSON User

-- >>> type Ex4 = "users" :? Param "author" String :> GetJSON User

-- >>> type Ex5 = Param "name" String :> Get '[ ResBody JSON User, 'ResHeaders '[ "content" := String] ]

-- >>> type Ex6 = "users" :? '[ Params '["some" := Int, "name" := String], ReqBody JSON User ] :> GetJSON User
-- $setup
--
-- The doctests in this module are run with following preamble:
--
-- >>> :set -XPolyKinds
-- >>> :set -XTypeFamily
-- >>> import Network.Core.API.Internal
-- >>> import Network.Core.API.MediaType
-- >>> import Network.Core.API.TypeLevel
-- >>> import Network.Core.API.Verbs
