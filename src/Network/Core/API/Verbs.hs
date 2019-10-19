module Network.Core.API.Verbs  where

import Data.Proxy (Proxy)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Types.Method (Method, StdMethod (..), methodConnect, methodDelete, methodGet,
                                  methodHead, methodOptions, methodPatch, methodPost, methodPut,
                                  methodTrace)

data Verb (method :: StdMethod) (contents:: [k])
  deriving (Typeable, Generic)

type Get ts = Verb 'GET ts

type Post ts  = Verb 'POST  ts

type Put ts  = Verb 'PUT ts

type Delete ts = Verb 'DELETE ts

type Patch ts = Verb 'PATCH  ts

class ReflectMethod a where
    reflectMethod :: Proxy a -> Method

instance ReflectMethod 'GET where
    reflectMethod _ = methodGet

instance ReflectMethod 'POST where
    reflectMethod _ = methodPost

instance ReflectMethod 'PUT where
    reflectMethod _ = methodPut

instance ReflectMethod 'DELETE where
    reflectMethod _ = methodDelete

instance ReflectMethod 'PATCH where
    reflectMethod _ = methodPatch

instance ReflectMethod 'HEAD where
    reflectMethod _ = methodHead

instance ReflectMethod 'OPTIONS where
    reflectMethod _ = methodOptions

instance ReflectMethod 'TRACE where
    reflectMethod _ = methodTrace

instance ReflectMethod 'CONNECT where
    reflectMethod _ = methodConnect
