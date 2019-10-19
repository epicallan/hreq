module Network.Core.Http.HttpError where

import Control.Exception

data HttpError = HttpError -- TODO: add error constructors
  deriving (Show, Eq)

instance Exception HttpError
