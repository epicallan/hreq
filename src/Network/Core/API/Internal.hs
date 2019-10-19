module Network.Core.API.Internal where

import Data.Singletons
import Data.Singletons.TypeLits

type Header a = (Symbol, a) -- (headerName, Type)

data ReqContent a =
      Params [(Symbol, a)]
    | QueryFlags [Symbol]
    -- | Captures [(Symbol, a)] -- we can have a type synonym for
    -- a single catpure from this
    -- CaptureAll Symbol
    -- TODO: add Capture and CaptureAll
    | ReqBody a a
    | ReqHeaders [Header a] -- none case sensitive header name -- TODO: list

data instance Sing (a :: ReqContent k) where
  SParams :: Sing ts -> Sing ('Params ts)
  SQueryFlags :: Sing ts -> Sing ('QueryFlags ts)
  SReqBody :: Sing ctyp -> Sing a -> Sing ('ReqBody ctyp a)
  SReqHeaders :: Sing a -> Sing ('ReqHeaders a)

instance (SingI ts) => SingI ('Params ts) where
  sing = SParams sing

instance (SingI ts) => SingI ('QueryFlags ts) where
  sing = SQueryFlags sing

instance (SingI a, SingI ctyp) => SingI ('ReqBody ctyp a) where
  sing = SReqBody sing sing

instance SingI a => SingI ('ReqHeaders a) where
  sing = SReqHeaders sing

data ResContent a =
    ResBody a a
  | ResHeaders [Header a] -- if you want to retrieve Headers, 'a' stands for decoding type
  | ResStatus -- ^ if you want to retrieve status
  -- ^ TODO: we need to add Raw

data instance Sing (a :: ResContent k) where
  SResBody :: Sing ctyp -> Sing a -> Sing ('ResBody ctyp a)
  SResHeaders :: Sing ts -> Sing ('ResHeaders ts)
  SResStatus :: Sing 'ResStatus

instance (SingI ctyp, SingI a) => SingI ('ResBody ctyp a) where
  sing = SResBody sing sing

instance SingI ts => SingI ('ResHeaders ts) where
  sing = SResHeaders sing

instance SingI 'ResStatus where
  sing = SResStatus

data Api a =
    Req [ ReqContent a]
  | Res [ ResContent a ] -- a declaration of what you need from response

data instance Sing (a :: Api k) where
  SReq :: Sing content -> Sing ('Req content)
  SRes :: Sing content -> Sing ('Res content)

instance (SingI content) => SingI ('Res content) where
  sing = SRes sing

instance (SingI content) => SingI ('Req content) where
  sing = SReq sing
