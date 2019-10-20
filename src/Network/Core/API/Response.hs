module Network.Core.API.Response where

import Data.Singletons
import GHC.TypeLits

data ResContent a =
    ResBody a a
  | ResHeaders [(Symbol, a)]
  | ResStatus
  | Raw

type ResBody = 'ResBody
type ResHeaders = 'ResHeaders
type ResStatus = 'ResStatus
type Raw = 'Raw

data instance Sing (a :: ResContent k) where
  SResBody :: Sing ctyp -> Sing a -> Sing ('ResBody ctyp a)
  SResHeaders :: Sing ts -> Sing ('ResHeaders ts)
  SResStatus :: Sing 'ResStatus
  SRaw :: Sing 'Raw

instance (SingI ctyp, SingI a) => SingI ('ResBody ctyp a) where
  sing = SResBody sing sing

instance SingI ts => SingI ('ResHeaders ts) where
  sing = SResHeaders sing

instance SingI 'Raw where
  sing = SRaw

instance SingI 'ResStatus where
  sing = SResStatus