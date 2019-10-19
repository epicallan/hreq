{-# LANGUAGE DeriveAnyClass  #-}
module Main where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Hreq

main :: IO ()
main = do
  res <- runHreq defaultConfig $ do
    x <- getExample
    return x
  print res
  -- where
  --   config = defaultConfig { base = Just "http://trequest.free.beeceptor.com" }

getExample :: RunHttp m => m Value
getExample = hreq @("user" :? QueryFlag "age" :> GetJSON Value) EmptyReq

data User = User
  { name :: String
  , age  :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)
