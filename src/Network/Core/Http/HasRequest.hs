{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeApplications     #-}
module Network.Core.Http.HasRequest where

import Data.Kind
import Data.List (foldr)
import Data.Proxy
import Data.Singletons.Prelude hiding (All)
import Data.Singletons.TypeLits
import Data.String (fromString)
import Data.String.Conversions (cs)
import Web.HttpApiData (ToHttpApiData (..))

import Network.Core.API
import Network.Core.Http.Hlist
import Network.Core.Http.Request
import Network.HTTP.Types (QueryItem)

pattern Empty :: Hlist '[]
pattern Empty = Nil

-- | We need the verb api component 'Verb' so as to access
-- Request method and content-type value
class HasRequest (reqComponents :: k) (verb :: Type) where
   type HttpInput reqComponents :: Type

   httpReq
     :: Proxy verb
     -> Proxy reqComponents
     -> HttpInput reqComponents
     -> Request
     -> Request

instance
   ( HttpReqConstraints ts
   , ReflectMethod method
   , SingI ('Req ts)
   , SingI ('Res rs)
   , HttpResConstraints rs
  )
  => HasRequest (ts :: [ReqContent Type]) (Verb method rs) where
  type HttpInput ts = Hlist (HttpReq ts)

  httpReq _ _ input req =
    let method'      = reflectMethod (Proxy @method)
        acceptHeader = case sing @('Res rs) of
                         SRes ys -> getAcceptHeader ys
        req'         = case sing @('Req ts) of
                         SReq xs -> encodeHlistAsReq xs input req

    in appendMethod method' $ req' { reqAccept = acceptHeader }

getAcceptHeader
  :: forall (rs :: [ ResContent Type]) . HttpResConstraints rs
  => Sing rs -> Maybe MediaType
getAcceptHeader = \case
  SNil -> Nothing
  SCons (SResBody sctyp _a) _rs -> Just $ mediaType sctyp
  SCons (SRaw _) rs -> getAcceptHeader rs
  SCons (SResHeaders _) rs -> getAcceptHeader rs

encodeHlistAsReq
  :: forall (ts :: [ReqContent Type]) . HttpReqConstraints ts
  => Sing ts
  -> Hlist (HttpReq ts)
  -> Request
  -> Request
encodeHlistAsReq xs input req = case (xs, input) of
  (SNil, _)                                                    ->
    req
  (SCons (SPath _ spath) sxs, ys) ->
    let path = withKnownSymbol spath (symbolVal spath)
        req' = appendToPath path req
    in encodeHlistAsReq sxs ys req'

  (SCons (SReqHeaders (SCons (STuple2 s _x) hs)) sxs, y :. ys) ->
    let headerName = fromString $ withKnownSymbol s (symbolVal s)
        req' = addHeader headerName y req
    in encodeHlistAsReq (SCons (SReqHeaders hs) sxs) ys req'

  (SCons (SReqHeaders SNil) sxs, ys)                           ->
    encodeHlistAsReq sxs ys req

  (SCons (SCaptureAll _a) sxs, y :. ys)                        ->
    let req' = appendToPath (cs $ toUrlPiece y) req
    in encodeHlistAsReq sxs ys req'

  (SCons (SCaptures SNil) sxs, ys)                             ->
    encodeHlistAsReq sxs ys req

  (SCons (SCaptures (SCons (STuple2 _s _x) zs)) sxs, y :. ys)  ->
    let req' = appendToPath (cs $ toUrlPiece y) req
    in encodeHlistAsReq (SCons (SCaptures zs) sxs) ys req'

  (SCons (SParams SNil) sxs, ys)                               ->
    encodeHlistAsReq sxs ys req

  (SCons (SParams (SCons (STuple2 s _x) ps)) sxs, y :. ys)     ->
    let req' = appendToQueryString (createParam s y) req
    in encodeHlistAsReq (SCons (SParams ps) sxs) ys req'

  (SCons (SQueryFlags _a sflags) SNil, _)                      ->
    appendQueryFlags (toQueryFlags sflags) req

  (SCons (SQueryFlags _a sflags) sxs, ys)                      ->
     encodeHlistAsReq sxs ys
       $ appendQueryFlags (toQueryFlags sflags) req

  (SCons (SReqBody sctyp _sa) sxs, y :. ys)                    ->
     let req' = setReqBody (encode sctyp y) (mediaType sctyp) req
     in encodeHlistAsReq sxs ys req'


{-------------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------------}

createParam
  :: (KnownSymbol p, ToHttpApiData a) => Sing p -> a -> QueryItem
createParam sname val =
  let pname = withKnownSymbol sname (symbolVal sname)
      value = toQueryParam val
  in (cs pname, Just $ cs value)

appendQueryFlags :: [String] -> Request -> Request
appendQueryFlags xs req =
  let queryflags = (\ x -> (cs x, Nothing)) <$> xs
  in foldr appendToQueryString req queryflags

toQueryFlags
  :: forall (fs :: [Symbol]) . All KnownSymbol fs
  => Sing fs
  -> [String]
toQueryFlags  = \case
  SNil -> []
  SCons x xs -> withKnownSymbol x (symbolVal x) : toQueryFlags xs
