{-# LANGUAGE OverloadedStrings, CPP #-}

module JsonUtils where

import Data.Functor.Identity
import qualified Data.Text as T
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as KM
#endif
import Data.Aeson
import Data.List (foldl')

#if !MIN_VERSION_aeson(2,0,0)
type Key = T.Text
stringToKey = T.pack
#else
stringToKey = K.fromString
#endif

mapObject :: (Object -> Object) -> Value -> Value
mapObject f (Object m) = Object (f m)
mapObject _ v =  error $ "Not an object " ++ show v

delete :: [Key] -> Value -> Value
delete []       = error "delete []"
delete ["*"]    = error "delete [\"*\"]"
delete [k]      = mapObject $ KM.delete k
delete ("*":ks) = mapObject $ KM.map (delete ks)
-- TODO: This is likely slower than using the underlying map's adjust function
delete (k:ks)   = mapObject $ runIdentity . KM.alterF (Identity . fmap (delete ks)) k

merge :: Value -> Value -> Value
merge (Object m1) (Object m2) = Object $ KM.unionWith merge m2 m1
merge v1 v2 = error $ "Cannot merge " ++ show v1 ++ " with " ++ show v2

merges :: [Value] -> Value
merges = foldl' merge (Object KM.empty)



