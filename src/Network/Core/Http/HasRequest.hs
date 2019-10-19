{-# LANGUAGE PatternSynonyms #-}
module Network.Core.Http.HasRequest where

import Data.Kind
import Data.List (foldl')
import Data.Proxy
import Data.Singletons.Prelude hiding (All)
import Data.Singletons.TypeLits
import Data.String.Conversions (cs)
import Network.Core.API
import Network.Core.Http.Hlist
import Network.Core.Http.Request
import Network.Core.Http.Response
import Network.Core.Http.RunHttp
import Network.HTTP.Types (QueryItem)
import Web.HttpApiData (ToHttpApiData (..))

pattern EmptyReq :: Hlist '[]
pattern EmptyReq = Nil

class RunHttp m => HasRequest api m where
   type HttpInput (api :: k) :: Type
   httpInput :: sing api -> HttpInput api -> Request -> m Response

instance
  ( KnownSymbol path
  , RunHttp m
  , HasRequest subroute m
  ) => HasRequest (path :? subroute) m where
  type HttpInput (path :? subroute) = HttpInput subroute

  httpInput subroute input reqContext = do
    let reqContext' = reqContext { reqPath = symbolVal (Proxy @path) }
    httpInput subroute input reqContext'

instance
  ( HttpReqConstraints ts
  , UniqMembers ts "Request Content"
  , ReflectMethod method
  , SingI ('Req ts)
  , RunHttp m
  )
  => HasRequest (ts :> Verb method rs) m where

  type HttpInput (ts :> Verb method rs) = Hlist (HttpReq ts)

  httpInput _ input req = case sing @ ('Req ts ) of
    SReq xs -> do
      let req' = encodeHlistAsReq xs input req
      httpInput (Proxy @(Verb method rs)) Nil req'

instance (RunHttp m, ReflectMethod  method)
  => HasRequest (Verb method rs) m where

  type HttpInput (Verb method rs) = Hlist '[]

  httpInput _ _ req = do
    let method' = reflectMethod (Proxy @(method))
    runRequest (appendMethod method' req)

encodeHlistAsReq
  :: forall (ts :: [ReqContent Type]) . HttpReqConstraints ts
  => Sing ts
  -> Hlist (HttpReq ts)
  -> Request
  -> Request
encodeHlistAsReq xs input req = case (xs, input) of
  (SNil, _)                                 -> req

  (SCons (SParams SNil) sxs, ys)            ->
    encodeHlistAsReq sxs ys req

  (SCons (SParams (SCons (STuple2 s _x) ps)) sxs, (y :. ys)) ->
    let req' = appendToQueryString (createParam s y) req
    in encodeHlistAsReq (SCons (SParams ps) sxs) ys req'

  (SCons (SQueryFlags sflags) SNil, _)       ->
    appendQueryFlags (toQueryFlags sflags) req

  (SCons (SQueryFlags sflags) sxs, ys)       ->
     encodeHlistAsReq sxs ys
       $ appendQueryFlags (toQueryFlags sflags) req

  (SCons (SReqBody sctyp _sa) sxs, (y :. ys)) ->
     encodeHlistAsReq sxs ys $ appendBody sctyp y req

  _                                           ->
    error "TODO: implement me"

appendBody
  :: (HasMediaType ctyp, MediaEncode ctyp a)
  => Sing ctyp -> a -> Request -> Request
appendBody sctyp x req = setReqBody (encode sctyp x) req

createParam
  :: (KnownSymbol p, ToHttpApiData a) => Sing p -> a -> QueryItem
createParam sname val =
  let pname = withKnownSymbol sname (symbolVal sname)
      value = toQueryParam val
  in (cs pname, Just $ cs value)

appendQueryFlags :: [String] -> Request -> Request
appendQueryFlags xs req =
  let queryflags = (\ x -> (cs x, Nothing)) <$> xs
  in foldl' (flip appendToQueryString) req queryflags
  -- ^ TODO:  use a foldl or foldr

toQueryFlags
  :: forall (fs :: [Symbol]) . All KnownSymbol fs
  => Sing fs
  -> [String]
toQueryFlags  = \case
  SNil -> []
  SCons x xs -> withKnownSymbol x (symbolVal x) : toQueryFlags xs
