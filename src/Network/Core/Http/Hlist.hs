module Network.Core.Http.Hlist where


import Data.Kind (Type)

infixr 7 :.

data Hlist (a :: [Type]) where
  Nil :: Hlist '[]
  (:.) :: x -> Hlist xs -> Hlist (x ': xs)
