module Network.Core.API.Response where

import Data.Singletons
import GHC.TypeLits

data ResContent a =
    ResBody a a
  | ResHeaders [(Symbol, a)]
  | Raw a -- ^ The a parameter is not used its just here to appease type GODs

type ResBody = 'ResBody
type ResHeaders = 'ResHeaders
type Raw = 'Raw

data instance Sing (a :: ResContent k) where
  SResBody :: Sing ctyp -> Sing a -> Sing ('ResBody ctyp a)
  SResHeaders :: Sing ts -> Sing ('ResHeaders ts)
  SRaw :: Sing a -> Sing ('Raw a)

instance (SingI ctyp, SingI a) => SingI ('ResBody ctyp a) where
  sing = SResBody sing sing

instance SingI ts => SingI ('ResHeaders ts) where
  sing = SResHeaders sing

instance SingI a => SingI ('Raw a) where
  sing = SRaw sing