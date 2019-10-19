module Network.HTTP.Hreq.Internal where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Singletons.TypeRepTYPE ()
import Network.Core.Http

import Network.HTTP.Hreq.Config

newtype Hreq m a = Treq { runHreq' :: ReaderT HttpConfig m a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader HttpConfig
           )

instance MonadError HttpError (Hreq IO) where
  throwError = throwError
  catchError = catchError

instance RunHttp (Hreq IO) where
  runRequest = undefined

  throwHttpError = throwError

runHreq :: HttpConfig -> Hreq m a -> m a
runHreq config = flip runReaderT config . runHreq'

runHreqDef :: Hreq m a -> m a
runHreqDef = runHreq defaultConfig
