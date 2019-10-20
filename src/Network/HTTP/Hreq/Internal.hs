module Network.HTTP.Hreq.Internal where

import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Lazy as LBS
import Data.Singletons.TypeRepTYPE ()
import Data.String.Conversions (cs)
import Network.Core.Http
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Hreq.Config
import Network.HTTP.Types (renderQuery)

newtype Hreq m a = Hreq { runHreq' :: ReaderT HttpConfig m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HttpConfig)

instance MonadError HttpError (Hreq IO) where
  throwError = throwError
  catchError = catchError

instance RunHttp (Hreq IO) where
  runRequest req = do
    config <- ask
    let manager' = manager config
    let req' = requestToHTTPRequest (baseUrl config) req

    httpResponse <- liftIO $ HTTP.httpLbs req' manager'
    return $ toResponse httpResponse

  throwHttpError = throwError

-- TODO: MonadBaseControl instances

toResponse :: HTTP.Response LBS.ByteString -> Response
toResponse = error "TODO: implement me"

runHreq :: MonadIO m => BaseUrl -> Hreq m a -> m a
runHreq baseUrl hrequest = do
  config <- liftIO $ createDefConfig baseUrl

  runHreqWithConfig config hrequest

runHreqWithConfig :: HttpConfig -> Hreq m a -> m a
runHreqWithConfig config hrequest = runReaderT (runHreq' hrequest) config

requestToHTTPRequest :: BaseUrl -> Request -> HTTP.Request
requestToHTTPRequest burl r = HTTP.defaultRequest
    { HTTP.method = reqMethod r
    , HTTP.host = cs $ baseUrlHost burl
    , HTTP.port = baseUrlPort burl
    , HTTP.path = cs $ baseUrlPath burl <> reqPath r
    , HTTP.queryString = renderQuery True $ reqQueryString r
    -- TODO: add headers
    , HTTP.requestBody = maybe mempty HTTP.RequestBodyBS $ fst <$> reqBody r
    , HTTP.secure = isSecure
    }
  where
    isSecure = case baseUrlScheme burl of
        Http  -> False
        Https -> True
