{-# LANGUAGE PatternSynonyms #-}
module Network.Core.Http.HasResponse where

import Data.Kind
import Data.Singletons.Prelude
import Network.Core.API
import Network.Core.Http.Hlist
import Network.Core.Http.HttpError
import Network.Core.Http.Response
import Network.Core.Http.RunHttp

pattern NoResponse :: Hlist '[]
pattern NoResponse = Nil

class RunHttp m => HasResponse api m where
   type HttpOutput (api :: k) :: Type

   httpRes :: sing api -> Response -> m (HttpOutput api)

instance HasResponse subroute m
  => HasResponse (path :? subroute) m where
  type  HttpOutput (path :? subroute) = HttpOutput subroute
  httpRes _ response = httpRes (Proxy @subroute) response

instance {-# OVERLAPPABLE #-}
  ( UniqMembers rs "Response content"
  , HasResponse rs m
  )
  => HasResponse (ts :> Verb method rs ) m where
  type HttpOutput (ts :> Verb method rs) = HttpOutput rs

  httpRes _ response = httpRes (Proxy @rs) response

instance (RunHttp m) => HasResponse '[] m where
  type HttpOutput '[] = Hlist '[]

  httpRes _ _ = return NoResponse
  

instance {-# OVERLAPPING #-}
  ( HasMediaType ctyp
  , MediaDecode ctyp a
  , RunHttp m
  )
  => HasResponse '[ 'ResBody  ctyp a ] m where
  type HttpOutput '[ 'ResBody ctyp a ] = a

  httpRes _ response = decodeAsBody (Proxy @ctyp) response

-- | The following type instances below are overly restrictive to avoid
-- overlapping type family instance error.
instance {-# OVERLAPPING #-}
  ( HasMediaType ctyp
  , MediaDecode ctyp a
  , RunHttp m
  , SingI ('Res (r ': rs))
  , HttpResConstraints (r ': rs)
  ) => HasResponse ( 'ResBody ctyp a ': r ': rs ) m where
  type HttpOutput  ( 'ResBody ctyp a ': r ': rs) = Hlist ( a ': HttpRes (r ': rs))
  httpRes _ response = case sing @('Res (r ': rs)) of
    SRes xs -> do
      body <- decodeAsBody (Proxy @ctyp) response
      reset <- decodeAsHlist xs response
      return $ body :. reset

instance {-# OVERLAPPING #-}
  ( RunHttp m
  , HttpResConstraints ('ResStatus ': rs)
  , SingI ('Res ('ResStatus ': rs))
  ) => HasResponse ('ResStatus ': (rs :: [ResContent Type])) m where
  type HttpOutput ('ResStatus ': rs) = Hlist (HttpRes ('ResStatus ': rs))

  httpRes _ response = case sing @('Res ('ResStatus ': rs))  of
    SRes xs -> decodeAsHlist xs response

instance {-# OVERLAPPING #-}
  ( RunHttp m
  , SingI ('Res ('ResHeaders hs ': rs))
  , HttpResConstraints ('ResHeaders hs ': rs)
  ) => HasResponse ('ResHeaders hs ': rs) m where
  type HttpOutput ('ResHeaders hs : rs) = Hlist (HttpRes ('ResHeaders hs ': rs))

  httpRes _ response = case sing @('Res ('ResHeaders hs ': rs)) of
    SRes xs -> decodeAsHlist xs response

-- TODO: HasResponse (Verb method '[ Raw ])

decodeAsBody -- | TODO: see  decodeAs used in Servant
  :: (RunHttp m, MediaDecode ctyp a, HasMediaType ctyp )
  => sing ctyp
  -> Response
  -> m a
decodeAsBody proxy response = case decode proxy (resBody response) of
  Left _err -> throwHttpError HttpError
  Right val -> pure val

decodeAsHlist
  :: (RunHttp m, HttpResConstraints rs)
  => Sing rs
  -> Response
  -> m (Hlist (HttpRes rs))
decodeAsHlist srs response = case srs of
  SNil -> return Nil

  SCons (SResBody ctyp _a) xs -> do
    body <- decodeAsBody ctyp response
    rest <- decodeAsHlist xs response
    return $ body :. rest

  SCons (SResHeaders (SCons _h _hs)) xs -> do
    let headers = resHeaders response
    rest <- decodeAsHlist xs response
    return $ headers :. rest

  SCons (SResHeaders SNil) xs ->
    decodeAsHlist xs response

  SCons SResStatus xs -> do
    let status = resStatus response
    rest <- decodeAsHlist xs response
    return $ status :. rest
