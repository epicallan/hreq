-- | Specification for valid Request component types used at the Kind and Type level for
-- creating an API structure.
--
module Hreq.Core.API.Request where

import Data.Kind (Type)
import Data.Singletons (Sing, SingI(..))
import GHC.TypeLits (Symbol)

-- * Request Type
data ReqContent a =
      Path a Symbol
    | Params [(Symbol, a)]
    | QueryFlags a [Symbol]
    | Captures [a]
    | BasicAuth a Symbol
    | CaptureAll a
    | ReqBody a a
    | StreamBody a a
    | ReqHeaders [(Symbol, a)]

-- * Request Type Synonyms
type Captures = 'Captures
type QueryFlags = 'QueryFlags ()
type Params = 'Params
type ReqBody = 'ReqBody
type StreamBody = 'StreamBody
type ReqHeaders = 'ReqHeaders
type CaptureAll = 'CaptureAll
type BasicAuth = 'BasicAuth ()
type Path = 'Path ()
type QueryFlag (s :: Symbol) = QueryFlags '[ s ]
type Param s t = Params '[ '( s,  t ) ]
type Capture t = Captures '[ t ]

-- * Request as a Singleton GADT
data SReqContent (a :: ReqContent Type)
  = forall t s . a ~ 'Path t s => SPath (Sing t) (Sing s)
  | forall ts (b :: Type) . a ~ 'QueryFlags b ts => SQueryFlags (Sing b) (Sing ts)
  | forall b . a ~ 'CaptureAll b => SCaptureAll (Sing b)
  | forall ts . a ~ 'Captures ts => SCaptures (Sing ts)
  | forall ctyp b . a ~ 'ReqBody ctyp b => SReqBody (Sing ctyp) (Sing b)
  | forall ctyp b . a ~ 'StreamBody ctyp b => SStreamBody (Sing ctyp) (Sing b)
  | forall ts . a ~ ReqHeaders ts => SReqHeaders (Sing ts)
  | forall ts . a ~ Params ts => SParams (Sing ts)
  | forall b s . a ~ 'BasicAuth b s => SBasicAuth (Sing b) (Sing s)

type instance Sing = SReqContent

instance (SingI a, SingI s) => SingI ('BasicAuth a s :: ReqContent Type) where
  sing = SBasicAuth sing sing

instance (SingI a, SingI s) => SingI ('Path a s :: ReqContent Type) where
  sing = SPath sing sing

instance SingI ts => SingI ('Params ts :: ReqContent Type) where
  sing = SParams sing

instance (SingI a, SingI ts) => SingI ('QueryFlags a ts :: ReqContent Type) where
  sing = SQueryFlags sing sing

instance SingI ts => SingI ('Captures ts :: ReqContent Type) where
  sing = SCaptures sing

instance (SingI a) => SingI ('CaptureAll a :: ReqContent Type) where
  sing = SCaptureAll sing

instance (SingI a, SingI ctyp) => SingI ('ReqBody ctyp a :: ReqContent Type) where
  sing = SReqBody sing sing

instance (SingI a, SingI ctyp) => SingI ('StreamBody ctyp a :: ReqContent Type) where
  sing = SStreamBody sing sing

instance SingI a => SingI ('ReqHeaders a :: ReqContent Type) where
  sing = SReqHeaders sing
