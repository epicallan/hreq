-- | This module provides a 'HasResponse' class that Interprets
-- 'Verb' content into a particular api endpoint query result.
--
-- For instance @Verb GET '[]@ gets interpreted as an empty response of type Unit i.e @()@
--
module Hreq.Core.Http.HasResponse where

import Control.Monad.Except
import Data.Kind
import Data.Hlist
import Data.Proxy
import Data.Singletons
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import GHC.TypeLits
import Network.HTTP.Types (hContentType)

import Hreq.Core.API
import Hreq.Core.Http.HttpError
import Hreq.Core.Http.Response

class MonadError HttpError m => HasResponse (a :: k) m where
   type HttpOutput a :: Type

   httpRes :: sing a -> Response -> m (HttpOutput a)

instance
  ( UniqMembers rs "Response"
  , HasResponse rs m
  )
  => HasResponse (Verb method rs ) m where

  type HttpOutput (Verb method rs) = HttpOutput rs

  httpRes _ = httpRes (Proxy @rs)

instance (MonadError HttpError m) => HasResponse '[] m where
  type HttpOutput '[] = ()

  httpRes _ _ = return ()

instance {-# OVERLAPPING #-} (MonadError HttpError m)
  => HasResponse '[ 'Raw a ] m where
  type HttpOutput '[ 'Raw a ] = Response

  httpRes _ = return

instance {-# OVERLAPPING #-}
  MonadError HttpError m
  => HasResponse ('Raw a : r : rs) m where

  type HttpOutput ('Raw a : r : rs) =
    TypeError ('Text "Raw should be used only in a singleton list")

  httpRes _ _ =  error "GHC Error"

instance {-# OVERLAPPING #-}
  ( MediaDecode ctyp a
  , MonadError HttpError m
  )
  => HasResponse '[ 'ResBody  ctyp a ] m where
  type HttpOutput '[ 'ResBody ctyp a ] = a

  httpRes _ = decodeAsBody (Proxy @ctyp)

-- | The following type instance is overly restrictive to avoid
-- overlapping type family instance error.
instance {-# OVERLAPPING #-}
  ( MediaDecode ctyp a
  , MonadError HttpError m
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
  ( MonadError HttpError m
  , SingI ('Res ('ResHeaders hs ': rs))
  , HttpResConstraints ('ResHeaders hs ': rs)
  ) => HasResponse ('ResHeaders hs ': rs) m where
  type HttpOutput ('ResHeaders hs : rs) = Hlist (HttpRes ('ResHeaders hs ': rs))

  httpRes _ response = case sing @('Res ('ResHeaders hs ': rs)) of
    SRes xs -> decodeAsHlist xs response

decodeAsBody
  :: forall ctyp a m sing
  . (MonadError HttpError m, MediaDecode ctyp a)
  => sing ctyp
  -> Response
  -> m a
decodeAsBody _ response = do
  responseContentType <- checkContentType

  unless (any (responseContentType `matches`) accepts)
    . throwError $ UnsupportedContentType (NE.head accepts) response

  case mediaDecode ctypProxy (LBS.toStrict $ resBody response) of
     Left err -> throwError $ DecodeFailure (unDecodeError err) response
     Right val -> pure val
  where
    ctypProxy :: Proxy ctyp
    ctypProxy = Proxy

    accepts :: NE.NonEmpty MediaType
    accepts = mediaTypes ctypProxy

    checkContentType :: m MediaType
    checkContentType  = case lookup hContentType $ resHeaders response of
      Nothing -> return $ mediaType (Proxy @PlainText) -- fall back to plain text
      Just t  -> maybe (throwError $ InvalidContentTypeHeader response) return $ parseAccept t

decodeAsHlist
  :: (MonadError HttpError m, HttpResConstraints rs)
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

  -- Should never match because we have a class instance
  -- that triggers a type error when 'Raw' is in a non-singleton
  -- type level list
  (SCons (SRaw _) _xs)-> error "GHC Error"